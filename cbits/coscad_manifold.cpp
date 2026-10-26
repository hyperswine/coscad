// Small owned ABI: no C++ exception or C++ allocation ownership crosses into
// Haskell. The native backend uses indexed vertices/faces. Legacy soup entry
// points remain for import/compatibility and explicitly weld exact positions.
#include <manifold/manifold.h>
#include <manifold/mesh.h>
#include <manifold/version.h>
#include <array>
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <map>
#include <memory>
#include <numeric>
#include <cstdint>
#include <vector>
#include <thread>
#include <chrono>
#include <condition_variable>
#include <mutex>
#include <stdexcept>

using manifold::Manifold;
using manifold::MeshGL64;
static_assert(MANIFOLD_VERSION == MANIFOLD_VERSION_NUMBER(3,5,2),
              "CoScad requires the version in native-kernel.lock.json");

namespace {
constexpr size_t max_triangles = 500000;

std::string status_name(Manifold::Error status) {
  switch (status) {
    case Manifold::Error::NotManifold: return "mesh is not a closed manifold";
    case Manifold::Error::NonFiniteVertex: return "non-finite vertex";
    case Manifold::Error::InvalidConstruction: return "invalid construction";
    case Manifold::Error::ResultTooLarge: return "result too large";
    case Manifold::Error::Cancelled: return "operation cancelled";
    default: return "kernel status " + std::to_string(static_cast<int>(status));
  }
}

void write_mesh(const Manifold &result, double **output, size_t *count) {
    const auto mesh = result.GetMeshGL64();
    const size_t n = mesh.NumTri();
    if (n > max_triangles) throw std::runtime_error("output exceeds 500000 triangles");
    auto buffer = std::make_unique<double[]>(9*n);
    for (size_t i = 0; i < 3*n; ++i) {
      const auto vertex = mesh.triVerts[i];
      for (size_t d = 0; d < 3; ++d) {
        const double v = mesh.vertProperties[mesh.numProp*vertex+d];
        if (!std::isfinite(v)) throw std::runtime_error("non-finite output vertex");
        buffer[3*i+d] = v;
      }
    }
    *count = n;
    *output = buffer.release();
}

Manifold read_mesh(const double *data, size_t count) {
  if (count > max_triangles || (count && !data))
    throw std::runtime_error("invalid or oversized input mesh");
  MeshGL64 mesh;
  std::map<std::array<double, 3>, uint64_t> index;
  for (size_t i = 0; i < 3 * count; ++i) {
    std::array<double, 3> p{data[3*i], data[3*i+1], data[3*i+2]};
    for (double v : p)
      if (!std::isfinite(v)) throw std::runtime_error("non-finite mesh vertex");
    const auto entry = index.emplace(p, index.size());
    if (entry.second)
      mesh.vertProperties.insert(mesh.vertProperties.end(), p.begin(), p.end());
    mesh.triVerts.push_back(entry.first->second);
  }
  Manifold result(mesh);
  const auto status = result.Status();
  if (status != Manifold::Error::NoError)
    throw std::runtime_error(std::string("input: ") + status_name(status));
  return result;
}
}

extern "C" int coscad_mesh_op(
    int op, const double *a, size_t na, const double *b, size_t nb,
    double **output, size_t *count, char *error, size_t error_size) noexcept {
  if (!output || !count || (error_size && !error)) return 1;
  *output = nullptr;
  *count = 0;
  if (error_size) error[0] = '\0';
  try {
    const auto left = read_mesh(a, na);
    const auto right = read_mesh(b, nb);
    Manifold result;
    switch (op) {
      case 0: result = left + right; break;
      case 1: result = left - right; break;
      case 2: result = left ^ right; break;
      case 3: result = Manifold::Hull(std::vector<Manifold>{left, right}); break;
      case 4: result = left.MinkowskiSum(right); break;
      default: throw std::runtime_error("unknown mesh operation");
    }
    const auto status = result.Status();
    if (status != Manifold::Error::NoError)
      throw std::runtime_error(std::string("operation: ") + status_name(status));
    write_mesh(result, output, count);
    return 0;
  } catch (const std::exception &e) {
    if (error_size) std::snprintf(error, error_size, "%s", e.what());
  } catch (...) {
    if (error_size) std::snprintf(error, error_size, "unknown C++ geometry failure");
  }
  return 1;
}

namespace {
// Cooperative kernel cancellation, not a process-level hard deadline. In
// particular one large Boolean may finish its current kernel phase first.
class OperationDeadline {
 public:
  manifold::ExecutionContext context;
  explicit OperationDeadline(uint64_t milliseconds) : timer([this,milliseconds] {
    std::unique_lock<std::mutex> lock(mutex);
    if (!condition.wait_for(lock,std::chrono::milliseconds(milliseconds),[this]{return done;}))
      context.Cancel();
  }) {}
  ~OperationDeadline() {
    { std::lock_guard<std::mutex> lock(mutex); done=true; }
    condition.notify_one(); timer.join();
  }
 private:
  std::mutex mutex;
  std::condition_variable condition;
  bool done=false;
  std::thread timer;
};

void check_deadline(const manifold::ExecutionContext &context) {
  if (context.Cancelled()) throw std::runtime_error("operation deadline exceeded (cooperative cancellation)");
}

Manifold read_indexed(const double *vertices, size_t nv, const uint64_t *faces, size_t nt, manifold::ExecutionContext &context) {
  check_deadline(context);
  if (nv>3*max_triangles || nt>max_triangles || (nv && !vertices) || (nt && !faces))
    throw std::runtime_error("invalid or oversized indexed mesh");
  MeshGL64 mesh;
  if (nv) mesh.vertProperties.assign(vertices,vertices+3*nv);
  for (double v : mesh.vertProperties)
    if (!std::isfinite(v)) throw std::runtime_error("non-finite indexed vertex");
  if (nt) mesh.triVerts.assign(faces,faces+3*nt);
  for (uint64_t v : mesh.triVerts)
    if (v>=nv) throw std::runtime_error("indexed triangle vertex out of range");
  // The kernel importer may simplify almost coincident opposing surfaces.
  // Refuse measurable volume drift instead of silently changing an operand.
  long double volume6=0,compensation=0;
  for (size_t i=0; i<nt; ++i) {
    long double p[3][3];
    for (size_t corner=0;corner<3;++corner) for (size_t d=0;d<3;++d)
      p[corner][d]=static_cast<long double>(vertices[3*faces[3*i+corner]+d])-vertices[d];
    const auto &a=p[0],&b=p[1],&c=p[2];
    const long double term=a[0]*(b[1]*c[2]-b[2]*c[1])+a[1]*(b[2]*c[0]-b[0]*c[2])+a[2]*(b[0]*c[1]-b[1]*c[0]);
    const long double corrected=term-compensation,next=volume6+corrected;
    compensation=(next-volume6)-corrected; volume6=next;
  }
  const double before=static_cast<double>(volume6/6);
  if (!std::isfinite(before)) throw std::runtime_error("indexed input volume is numerically unresolved");
  Manifold result=context.FromMeshGL(mesh);
  check_deadline(context);
  if (result.Status()!=Manifold::Error::NoError)
    throw std::runtime_error(std::string("indexed input: ")+status_name(result.Status()));
  if (std::abs(result.Volume()-before)>std::max(1e-8,std::abs(before)*1e-10))
    throw std::runtime_error("indexed import changed volume; near-coincident operand is unresolved");
  return result;
}

// Collapse only the kernel's declared property-vertex merges. Equal positions
// with no merge relation remain distinct physical vertices (point/edge contact).
void write_indexed(const Manifold &result, double **vertices, size_t *nv,
                   uint64_t **faces, size_t *nt, manifold::ExecutionContext &context) {
  check_deadline(context);
  const auto mesh=result.GetMeshGL64();
  if (mesh.NumTri()>max_triangles || mesh.NumVert()>3*max_triangles || mesh.numProp<3)
    throw std::runtime_error("indexed output exceeds mesh limits");
  std::vector<uint64_t> parent(mesh.NumVert());
  std::iota(parent.begin(),parent.end(),0);
  auto root=[&](uint64_t v) {
    if (v>=parent.size()) throw std::runtime_error("invalid kernel vertex index");
    while (parent[v]!=v) { parent[v]=parent[parent[v]]; v=parent[v]; }
    return v;
  };
  if (mesh.mergeFromVert.size()!=mesh.mergeToVert.size())
    throw std::runtime_error("invalid kernel merge relation");
  for (size_t i=0; i<mesh.mergeFromVert.size(); ++i) {
    const auto a=root(mesh.mergeFromVert[i]), b=root(mesh.mergeToVert[i]);
    parent[std::max(a,b)]=std::min(a,b);
  }
  std::map<uint64_t,uint64_t> compact;
  std::vector<double> points;
  auto indices=std::make_unique<uint64_t[]>(mesh.triVerts.size());
  for (size_t i=0; i<mesh.triVerts.size(); ++i) {
    const auto representative=root(mesh.triVerts[i]);
    auto entry=compact.emplace(representative,compact.size());
    if (entry.second) for (size_t d=0; d<3; ++d) {
      const double v=mesh.vertProperties[mesh.numProp*representative+d];
      if (!std::isfinite(v)) throw std::runtime_error("non-finite indexed output vertex");
      points.push_back(v);
    }
    indices[i]=entry.first->second;
  }
  MeshGL64 canonical;
  canonical.vertProperties=std::move(points);
  canonical.triVerts.assign(indices.get(),indices.get()+mesh.triVerts.size());
  // Do not re-import an ordinary result: the importer may simplify nearly
  // coincident opposing faces and change a thin Boolean result's volume.
  // Only normalize exact zero-area transient faces, e.g. after Minkowski.
  bool degenerate=false;
  for (size_t i=0; i<canonical.NumTri(); ++i) {
    const auto face=canonical.GetTriVerts(i);
    const auto a=canonical.GetVertPos(face.x),b=canonical.GetVertPos(face.y),c=canonical.GetVertPos(face.z);
    const auto normal=manifold::la::cross(b-a,c-a);
    if (manifold::la::dot(normal,normal)==0) { degenerate=true; break; }
  }
  MeshGL64 clean=canonical;
  if (degenerate) {
    check_deadline(context);
    const double before=result.Volume();
    Manifold normalized=context.FromMeshGL(canonical);
    check_deadline(context);
    if (normalized.Status()!=Manifold::Error::NoError)
      throw std::runtime_error(std::string("indexed normalization: ")+status_name(normalized.Status()));
    if (std::abs(normalized.Volume()-before)>std::max(1e-8,std::abs(before)*1e-10))
      throw std::runtime_error("indexed normalization changed volume by "+std::to_string(normalized.Volume()-before)+" mm3 from "+std::to_string(before)+"; no result accepted");
    clean=normalized.GetMeshGL64();
  }
  if (!clean.mergeFromVert.empty() || clean.numProp!=3 || clean.NumVert()>3*max_triangles || clean.NumTri()>max_triangles)
    throw std::runtime_error("unexpected indexed normalization layout");
  auto coordinates=std::make_unique<double[]>(clean.vertProperties.size());
  auto clean_indices=std::make_unique<uint64_t[]>(clean.triVerts.size());
  std::copy(clean.vertProperties.begin(),clean.vertProperties.end(),coordinates.get());
  std::copy(clean.triVerts.begin(),clean.triVerts.end(),clean_indices.get());
  check_deadline(context);
  *nv=clean.NumVert(); *nt=clean.NumTri();
  *vertices=coordinates.release(); *faces=clean_indices.release();
}
}

extern "C" int coscad_mesh_op_indexed_timed(
    uint64_t timeout_ms, int op, const double *av, size_t anv, const uint64_t *af, size_t ant,
    const double *bv, size_t bnv, const uint64_t *bf, size_t bnt,
    double **vertices, size_t *nv, uint64_t **faces, size_t *nt,
    char *error, size_t error_size) noexcept {
  if (!vertices || !nv || !faces || !nt || (error_size && !error)) return 1;
  *vertices=nullptr; *faces=nullptr; *nv=0; *nt=0;
  if (error_size) error[0]='\0';
  try {
    if (timeout_ms<1 || timeout_ms>600000) throw std::runtime_error("native timeout must be 1..600000 milliseconds");
    OperationDeadline deadline(timeout_ms);
    auto &context=deadline.context;
    const auto left=read_indexed(av,anv,af,ant,context), right=read_indexed(bv,bnv,bf,bnt,context);
    Manifold result;
    switch (op) {
      case 0: result=left+right; break;
      case 1: result=left-right; break;
      case 2: result=left^right; break;
      case 3: result=Manifold::Hull(std::vector<Manifold>{left,right}); break;
      case 4: result=left.WithContext(context).MinkowskiSum(right); break;
      default: throw std::runtime_error("unknown indexed mesh operation");
    }
    result=result.WithContext(context);
    const auto status=result.Status();
    check_deadline(context);
    if (status!=Manifold::Error::NoError)
      throw std::runtime_error(std::string("indexed operation: ")+status_name(status));
    write_indexed(result,vertices,nv,faces,nt,context);
    return 0;
  } catch (const std::exception &e) {
    if (error_size) std::snprintf(error,error_size,"%s",e.what());
  } catch (...) {
    if (error_size) std::snprintf(error,error_size,"unknown C++ indexed geometry failure");
  }
  return 1;
}

// Compatibility entry point shares the same default per-operation deadline.
extern "C" int coscad_mesh_op_indexed(
    int op, const double *av, size_t anv, const uint64_t *af, size_t ant,
    const double *bv, size_t bnv, const uint64_t *bf, size_t bnt,
    double **vertices, size_t *nv, uint64_t **faces, size_t *nt,
    char *error, size_t error_size) noexcept {
  return coscad_mesh_op_indexed_timed(60000,op,av,anv,af,ant,bv,bnv,bf,bnt,vertices,nv,faces,nt,error,error_size);
}

extern "C" void coscad_mesh_free_indices(uint64_t *p) noexcept { delete[] p; }

extern "C" void coscad_mesh_free(double *p) noexcept { delete[] p; }


// Single simple outline. Composite 2D operations are distributed over the
// extrusion by Haskell. Holes/offsets are refused until a profile contract exists.
extern "C" int coscad_mesh_extrude(const double *xy, size_t n, double height,
    double **output, size_t *count, char *error, size_t error_size) noexcept {
  if (!output || !count || (error_size && !error)) return 1;
  *output=nullptr; *count=0;
  if (error_size) error[0]='\0';
  try {
    if (!xy || n<3 || n>4096 || !std::isfinite(height) || height<=0 || height>1e9)
      throw std::runtime_error("extrusion requires 3..4096 vertices and a finite positive height <= 1e9 mm");
    manifold::SimplePolygon polygon;
    double scale=1, area=0;
    std::map<std::array<double,2>,bool> unique;
    for (size_t i=0; i<n; ++i) {
      const double x=xy[2*i], y=xy[2*i+1];
      if (!std::isfinite(x) || !std::isfinite(y) || std::abs(x)>1e9 || std::abs(y)>1e9)
        throw std::runtime_error("polygon coordinates must be finite and at most 1e9 mm");
      if (!unique.emplace(std::array<double,2>{x,y},true).second)
        throw std::runtime_error("polygon has repeated vertices");
      polygon.push_back({x,y});
      scale=std::max(scale,std::max(std::abs(x-xy[0]),std::abs(y-xy[1])));
    }
    const double eps=1e-12*scale*scale;
    auto orient=[](auto a,auto b,auto c) { return (b.x-a.x)*(c.y-a.y)-(b.y-a.y)*(c.x-a.x); };
    for (size_t i=0; i<n; ++i) area+=orient(polygon[0],polygon[i],polygon[(i+1)%n]);
    if (!std::isfinite(area) || std::abs(area)<=eps)
      throw std::runtime_error("polygon has zero or numerically unresolved area");
    // Reject touching/crossing non-neighbour edges, including a repeated point
    // in a different edge. The bounding-box test excludes collinear disjoint edges.
    for (size_t i=0; i<n; ++i) for (size_t j=i+1; j<n; ++j) {
      if (j==i+1 || (i==0 && j==n-1)) continue;
      const auto a=polygon[i],b=polygon[(i+1)%n],c=polygon[j],d=polygon[(j+1)%n];
      if (std::max(a.x,b.x)<std::min(c.x,d.x) || std::max(c.x,d.x)<std::min(a.x,b.x) ||
          std::max(a.y,b.y)<std::min(c.y,d.y) || std::max(c.y,d.y)<std::min(a.y,b.y)) continue;
      auto sign=[eps](double v) { return v>eps ? 1 : v < -eps ? -1 : 0; };
      if (sign(orient(a,b,c))*sign(orient(a,b,d))<=0 && sign(orient(c,d,a))*sign(orient(c,d,b))<=0)
        throw std::runtime_error("polygon self-intersects or touches a non-neighbour edge");
    }
    if (area<0) std::reverse(polygon.begin(),polygon.end());
    auto result=Manifold::Extrude({polygon},height);
    if (result.Status()!=Manifold::Error::NoError)
      throw std::runtime_error(std::string("extrusion: ")+status_name(result.Status()));
    write_mesh(result,output,count);
    return 0;
  } catch (const std::exception &e) {
    if (error_size) std::snprintf(error,error_size,"%s",e.what());
  } catch (...) {
    if (error_size) std::snprintf(error,error_size,"unknown C++ extrusion failure");
  }
  return 1;
}
