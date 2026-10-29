// Small owned ABI: no C++ exception or C++ allocation ownership crosses into
// Haskell. The native backend uses indexed vertices/faces. Legacy soup entry
// points remain for import/compatibility and explicitly weld exact positions.
#include <manifold/cross_section.h>
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


namespace {
// A single simple outline, validated (finite, bounded, no repeated vertex,
// nonzero area, no touching/crossing non-neighbour edges) and oriented
// counter-clockwise. Shared by plain and offset extrusions.
manifold::SimplePolygon read_polygon(const double *xy, size_t n) {
    if (!xy || n<3 || n>4096)
      throw std::runtime_error("extrusion requires 3..4096 vertices");
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
    return polygon;
}
}

// Round 2D offset (OpenSCAD `offset(r = delta)`) of a single simple outline,
// then a straight extrusion. The CrossSection backend is Clipper2, the same
// library OpenSCAD uses, with the arc segment count OpenSCAD derives from
// $fn; a negative delta shrinks. An offset that removes the whole profile
// is an error, never an empty success. Same ownership rules as the others.
extern "C" int coscad_mesh_extrude_offset(const double *xy, size_t n, double delta, int segments,
    double height, double **output, size_t *count, char *error, size_t error_size) noexcept {
  if (!output || !count || (error_size && !error)) return 1;
  *output=nullptr; *count=0;
  if (error_size) error[0]='\0';
  try {
    if (!std::isfinite(height) || height<=0 || height>1e9)
      throw std::runtime_error("extrusion requires a finite positive height <= 1e9 mm");
    if (!std::isfinite(delta) || std::abs(delta)>1e9)
      throw std::runtime_error("offset radius must be finite and at most 1e9 mm");
    if (segments<3 || segments>4096)
      throw std::runtime_error("offset arc segments must be 3..4096");
    const auto polygon=read_polygon(xy,n);
    manifold::CrossSection section(polygon);
    if (section.IsEmpty()) throw std::runtime_error("offset profile is empty after import");
    auto inflated=section;
    if (delta!=0) inflated=section.Offset(delta,manifold::CrossSection::JoinType::Round,2.0,segments);
    if (inflated.IsEmpty()) throw std::runtime_error("offset removed the whole profile (negative radius larger than the shape)");
    const auto polygons=inflated.ToPolygons();
    size_t total=0;
    for (const auto &contour : polygons) total+=contour.size();
    if (polygons.empty() || total<3 || total>65536)
      throw std::runtime_error("offset produced an unusable outline (empty or more than 65536 vertices)");
    for (const auto &contour : polygons) for (const auto &p : contour)
      if (!std::isfinite(p.x) || !std::isfinite(p.y)) throw std::runtime_error("offset produced a non-finite vertex");
    auto result=Manifold::Extrude(polygons,height);
    if (result.Status()!=Manifold::Error::NoError)
      throw std::runtime_error(std::string("offset extrusion: ")+status_name(result.Status()));
    if (result.IsEmpty()) throw std::runtime_error("offset extrusion produced an empty solid");
    write_mesh(result,output,count);
    return 0;
  } catch (const std::exception &e) {
    if (error_size) std::snprintf(error,error_size,"%s",e.what());
  } catch (...) {
    if (error_size) std::snprintf(error,error_size,"unknown C++ offset extrusion failure");
  }
  return 1;
}

// Single simple outline. Composite 2D operations are distributed over the
// extrusion by Haskell. Holes are refused until a profile contract exists.
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

// ------------------------------------------------------------------
// Persistent solids for the build planner: a part is imported once, then
// queried many times under rigid transforms (gap and overlap during an
// insertion sweep). The handle is owned by Haskell through a finalizer.
namespace {
// Triangle distance queries for the planner's sweeps. Manifold::MinGap
// first runs a full Boolean intersection on every call; a sweep makes
// hundreds of calls, so the planner keeps its own bounding-volume tree of
// each posed solid's triangles and searches two trees together.
struct V { double x, y, z; };
inline V operator+(V a, V b) { return {a.x+b.x, a.y+b.y, a.z+b.z}; }
inline V operator-(V a, V b) { return {a.x-b.x, a.y-b.y, a.z-b.z}; }
inline V operator*(double k, V a) { return {k*a.x, k*a.y, k*a.z}; }
inline double dot(V a, V b) { return a.x*b.x + a.y*b.y + a.z*b.z; }
inline V crossv(V a, V b) { return {a.y*b.z-a.z*b.y, a.z*b.x-a.x*b.z, a.x*b.y-a.y*b.x}; }
inline double clamp01(double t) { return t < 0 ? 0 : (t > 1 ? 1 : t); }

struct Box3 { V lo{1e300,1e300,1e300}, hi{-1e300,-1e300,-1e300};
  void add(V p) { lo = {std::min(lo.x,p.x), std::min(lo.y,p.y), std::min(lo.z,p.z)}; hi = {std::max(hi.x,p.x), std::max(hi.y,p.y), std::max(hi.z,p.z)}; } };

double box_gap2(const Box3 &a, const Box3 &b) {
  auto axis = [](double alo, double ahi, double blo, double bhi) { double d = std::max(0.0, std::max(blo - ahi, alo - bhi)); return d*d; };
  return axis(a.lo.x,a.hi.x,b.lo.x,b.hi.x) + axis(a.lo.y,a.hi.y,b.lo.y,b.hi.y) + axis(a.lo.z,a.hi.z,b.lo.z,b.hi.z);
}

// Ericson, Real-Time Collision Detection 5.1.5 and 5.1.9.
V closest_on_triangle(V p, V a, V b, V c) {
  V ab = b-a, ac = c-a, ap = p-a;
  double d1 = dot(ab,ap), d2 = dot(ac,ap);
  if (d1 <= 0 && d2 <= 0) return a;
  V bp = p-b; double d3 = dot(ab,bp), d4 = dot(ac,bp);
  if (d3 >= 0 && d4 <= d3) return b;
  double vc = d1*d4 - d3*d2;
  if (vc <= 0 && d1 >= 0 && d3 <= 0) return a + (d1/(d1-d3))*ab;
  V cp = p-c; double d5 = dot(ab,cp), d6 = dot(ac,cp);
  if (d6 >= 0 && d5 <= d6) return c;
  double vb = d5*d2 - d1*d6;
  if (vb <= 0 && d2 >= 0 && d6 <= 0) return a + (d2/(d2-d6))*ac;
  double va = d3*d6 - d5*d4;
  if (va <= 0 && (d4-d3) >= 0 && (d5-d6) >= 0) return b + ((d4-d3)/((d4-d3)+(d5-d6)))*(c-b);
  double denom = 1/(va+vb+vc);
  return a + (vb*denom)*ab + (vc*denom)*ac;
}

double segment_segment2(V p1, V q1, V p2, V q2) {
  V d1 = q1-p1, d2 = q2-p2, r = p1-p2;
  double a = dot(d1,d1), e = dot(d2,d2), f = dot(d2,r), s, t;
  if (a <= 1e-30 && e <= 1e-30) { V w = p1-p2; return dot(w,w); }
  if (a <= 1e-30) { s = 0; t = clamp01(f/e); }
  else { double c = dot(d1,r);
    if (e <= 1e-30) { t = 0; s = clamp01(-c/a); }
    else { double b = dot(d1,d2), denom = a*e - b*b;
      s = denom != 0 ? clamp01((b*f - c*e)/denom) : 0;
      t = (b*s + f)/e;
      if (t < 0) { t = 0; s = clamp01(-c/a); } else if (t > 1) { t = 1; s = clamp01((b-c)/a); } } }
  V w = (p1 + s*d1) - (p2 + t*d2);
  return dot(w,w);
}

bool segment_hits_triangle(V p, V q, V a, V b, V c) {
  V d = q-p, e1 = b-a, e2 = c-a, h = crossv(d,e2);
  double det = dot(e1,h);
  if (std::abs(det) < 1e-18) return false;
  double inv = 1/det; V sv = p-a;
  double u = inv*dot(sv,h); if (u < 0 || u > 1) return false;
  V qv = crossv(sv,e1); double v = inv*dot(d,qv); if (v < 0 || u+v > 1) return false;
  double t = inv*dot(e2,qv); return t >= 0 && t <= 1;
}

double triangle_distance2(const V *x, const V *y, bool boxes_overlap) {
  if (boxes_overlap) for (int i = 0; i < 3; ++i) {
    if (segment_hits_triangle(x[i], x[(i+1)%3], y[0], y[1], y[2])) return 0;
    if (segment_hits_triangle(y[i], y[(i+1)%3], x[0], x[1], x[2])) return 0;
  }
  double best = 1e300;
  for (int i = 0; i < 3; ++i) {
    V p = closest_on_triangle(x[i], y[0], y[1], y[2]); V w = p - x[i]; best = std::min(best, dot(w,w));
    V r = closest_on_triangle(y[i], x[0], x[1], x[2]); V z = r - y[i]; best = std::min(best, dot(z,z));
    for (int j = 0; j < 3; ++j) best = std::min(best, segment_segment2(x[i], x[(i+1)%3], y[j], y[(j+1)%3]));
  }
  return best;
}

struct Tree {
  std::vector<V> tri; // 3 per triangle, posed
  struct Node { Box3 box; int left = -1, right = -1, start = 0, count = 0; };
  std::vector<Node> nodes;
  std::vector<int> order;
  int build(int start, int count, std::vector<V> &centre) {
    Node n; for (int k = start; k < start+count; ++k) for (int c = 0; c < 3; ++c) n.box.add(tri[3*order[k]+c]);
    int id = static_cast<int>(nodes.size()); nodes.push_back(n);
    if (count <= 4) { nodes[id].start = start; nodes[id].count = count; return id; }
    V ext = n.box.hi - n.box.lo;
    int axis = ext.x >= ext.y && ext.x >= ext.z ? 0 : (ext.y >= ext.z ? 1 : 2);
    auto key = [&](int t) { V c = centre[t]; return axis == 0 ? c.x : (axis == 1 ? c.y : c.z); };
    std::nth_element(order.begin()+start, order.begin()+start+count/2, order.begin()+start+count, [&](int a, int b) { return key(a) < key(b); });
    int l = build(start, count/2, centre), r = build(start+count/2, count-count/2, centre);
    nodes[id].left = l; nodes[id].right = r;
    return id;
  }
  // inset > 0 moves every vertex that far inward along its area-weighted
  // normal: a slightly shrunken copy that clears whatever the solid only touches.
  Tree(const Manifold &m, double inset) {
    const auto mesh = m.GetMeshGL64();
    const size_t n = mesh.NumTri(), nv = mesh.NumVert();
    std::vector<V> pos(nv), normal(nv, V{0,0,0});
    for (size_t v = 0; v < nv; ++v) pos[v] = {mesh.vertProperties[mesh.numProp*v], mesh.vertProperties[mesh.numProp*v+1], mesh.vertProperties[mesh.numProp*v+2]};
    if (inset > 0) {
      for (size_t i = 0; i < n; ++i) {
        auto a = mesh.triVerts[3*i], b = mesh.triVerts[3*i+1], c = mesh.triVerts[3*i+2];
        V w = crossv(pos[b]-pos[a], pos[c]-pos[a]);
        normal[a] = normal[a] + w; normal[b] = normal[b] + w; normal[c] = normal[c] + w;
      }
      // the kernel splits vertices along sharp edges; join normals by position
      std::map<std::array<double,3>, V> byPos;
      for (size_t v = 0; v < nv; ++v) { auto &acc = byPos[{pos[v].x,pos[v].y,pos[v].z}]; acc = acc + normal[v]; }
      for (size_t v = 0; v < nv; ++v) {
        V w = byPos[{pos[v].x,pos[v].y,pos[v].z}]; double l = std::sqrt(dot(w,w));
        if (l > 0) pos[v] = pos[v] - (inset/l)*w;
      }
    }
    tri.resize(3*n);
    std::vector<V> centre(n);
    for (size_t i = 0; i < n; ++i) {
      for (int c = 0; c < 3; ++c) tri[3*i+c] = pos[mesh.triVerts[3*i+c]];
      centre[i] = (1.0/3) * (tri[3*i] + tri[3*i+1] + tri[3*i+2]);
    }
    order.resize(n); std::iota(order.begin(), order.end(), 0);
    if (n) build(0, static_cast<int>(n), centre);
  }
};

struct Solid { Manifold m; mutable std::unique_ptr<Tree> tree; mutable std::unique_ptr<Tree> shrunk; mutable double shrunkBy = 0;
  const Tree &get() const { if (!tree) tree = std::make_unique<Tree>(m, 0); return *tree; }
  const Tree &inset(double d) const { if (!shrunk || shrunkBy != d) { shrunk = std::make_unique<Tree>(m, d); shrunkBy = d; } return *shrunk; } };

// Smallest distance from a (moved rigidly by r, t) to b, up to search.
double tree_gap(const Tree &a, const double r[3][3], V t, const Tree &b, double search, double enough) {
  if (a.nodes.empty() || b.nodes.empty()) return search;
  auto move = [&](V p) { return V{r[0][0]*p.x+r[0][1]*p.y+r[0][2]*p.z+t.x, r[1][0]*p.x+r[1][1]*p.y+r[1][2]*p.z+t.y, r[2][0]*p.x+r[2][1]*p.y+r[2][2]*p.z+t.z}; };
  auto moveBox = [&](const Box3 &bx) {
    V c = 0.5*(bx.lo+bx.hi), e = 0.5*(bx.hi-bx.lo), mc = move(c);
    V he{std::abs(r[0][0])*e.x+std::abs(r[0][1])*e.y+std::abs(r[0][2])*e.z, std::abs(r[1][0])*e.x+std::abs(r[1][1])*e.y+std::abs(r[1][2])*e.z, std::abs(r[2][0])*e.x+std::abs(r[2][1])*e.y+std::abs(r[2][2])*e.z};
    Box3 o; o.lo = mc - he; o.hi = mc + he; return o; };
  double best2 = search*search;
  const double enough2 = enough*enough;
  thread_local std::vector<std::pair<int,int>> stack;
  stack.clear(); stack.push_back({0,0});
  auto boxA = [&](int i) { return moveBox(a.nodes[i].box); };
  while (!stack.empty()) {
    auto [ia, ib] = stack.back(); stack.pop_back();
    const auto &na = a.nodes[ia]; const auto &nb = b.nodes[ib];
    if (box_gap2(boxA(ia), nb.box) >= best2) continue;
    bool leafA = na.left < 0, leafB = nb.left < 0;
    if (leafA && leafB) {
      for (int i = na.start; i < na.start+na.count; ++i) {
        V x[3] = {move(a.tri[3*a.order[i]]), move(a.tri[3*a.order[i]+1]), move(a.tri[3*a.order[i]+2])};
        Box3 bx; for (auto &p : x) bx.add(p);
        for (int j = nb.start; j < nb.start+nb.count; ++j) {
          const V *y = &b.tri[3*b.order[j]];
          Box3 by; for (int c = 0; c < 3; ++c) by.add(y[c]);
          const double g2 = box_gap2(bx, by);
          if (g2 >= best2) continue;
          best2 = std::min(best2, triangle_distance2(x, y, g2 == 0));
          if (best2 <= enough2) return std::sqrt(best2);
        }
      }
    } else {
      // split the larger node; visit the nearer child pair first
      std::pair<int,int> c1, c2;
      if (leafB || (!leafA && (na.box.hi.x-na.box.lo.x)+(na.box.hi.y-na.box.lo.y)+(na.box.hi.z-na.box.lo.z) >= (nb.box.hi.x-nb.box.lo.x)+(nb.box.hi.y-nb.box.lo.y)+(nb.box.hi.z-nb.box.lo.z))) {
        c1 = {na.left, ib}; c2 = {na.right, ib};
      } else {
        c1 = {ia, nb.left}; c2 = {ia, nb.right};
      }
      double d1 = box_gap2(boxA(c1.first), b.nodes[c1.second].box), d2 = box_gap2(boxA(c2.first), b.nodes[c2.second].box);
      if (d1 <= d2) { if (d2 < best2) stack.push_back(c2); if (d1 < best2) stack.push_back(c1); }
      else { if (d1 < best2) stack.push_back(c1); if (d2 < best2) stack.push_back(c2); }
    }
  }
  return std::sqrt(best2);
}

bool identity_pose(const double *t) {
  static const double id[12] = {1,0,0,0, 0,1,0,0, 0,0,1,0};
  for (int i = 0; i < 12; ++i) if (t[i] != id[i]) return false;
  return true;
}

// Posing an already posed solid by the identity must not create a new
// node: the kernel caches the posed mesh and its collision tree there.
Manifold posed(const Manifold &m, const double *t);

manifold::mat3x4 read_pose(const double *t) {
  // row-major 3x4: [r11 r12 r13 tx; r21 r22 r23 ty; r31 r32 r33 tz]
  manifold::mat3x4 pose;
  for (int c = 0; c < 4; ++c)
    for (int r = 0; r < 3; ++r) {
      const double v = t[4*r+c];
      if (!std::isfinite(v)) throw std::runtime_error("non-finite pose");
      pose[c][r] = v;
    }
  return pose;
}
}

extern "C" int coscad_solid_new(const double *v, size_t nv, const uint64_t *f, size_t nt,
                                void **out, char *error, size_t error_size) noexcept {
  if (!out || (error_size && !error)) return 1;
  *out = nullptr;
  if (error_size) error[0] = '\0';
  try {
    if (nv>3*max_triangles || nt>max_triangles || (nv && !v) || (nt && !f))
      throw std::runtime_error("invalid or oversized solid mesh");
    MeshGL64 mesh;
    if (nv) mesh.vertProperties.assign(v, v+3*nv);
    if (nt) mesh.triVerts.assign(f, f+3*nt);
    for (uint64_t i : mesh.triVerts) if (i >= nv) throw std::runtime_error("solid face index out of range");
    Manifold m(mesh);
    if (m.Status() != Manifold::Error::NoError)
      throw std::runtime_error(std::string("solid: ")+status_name(m.Status()));
    *out = new Solid{m, nullptr};
    return 0;
  } catch (const std::exception &e) {
    if (error_size) std::snprintf(error, error_size, "%s", e.what());
  } catch (...) {
    if (error_size) std::snprintf(error, error_size, "unknown C++ solid failure");
  }
  return 1;
}

extern "C" void coscad_solid_free(void *p) noexcept { delete static_cast<Solid*>(p); }

// Build the distance trees now (the full one and, for inset > 0, the
// shrunken one), so that later queries from several threads only read them.
extern "C" int coscad_solid_prepare(const void *a, double inset) noexcept {
  if (!a) return 1;
  try {
    const auto &s = *static_cast<const Solid*>(a);
    (void)s.get();
    if (inset > 0) (void)s.inset(inset);
    return 0;
  } catch (...) { return 1; }
}

namespace {
Manifold posed(const Manifold &m, const double *t) {
  return identity_pose(t) ? m : m.Transform(read_pose(t));
}
}

// A new solid: this one under a pose, realised once so later queries reuse
// its posed mesh and collision tree.
extern "C" int coscad_solid_posed(const void *a, const double *ta, void **out, char *error, size_t error_size) noexcept {
  if (!a || !ta || !out || (error_size && !error)) return 1;
  *out = nullptr;
  if (error_size) error[0] = '\0';
  try {
    Manifold m = static_cast<const Solid*>(a)->m.Transform(read_pose(ta));
    if (m.Status() != Manifold::Error::NoError)
      throw std::runtime_error(std::string("posed solid: ")+status_name(m.Status()));
    (void)m.NumVert();
    *out = new Solid{m, nullptr};
    return 0;
  } catch (const std::exception &e) {
    if (error_size) std::snprintf(error, error_size, "%s", e.what());
  } catch (...) {
    if (error_size) std::snprintf(error, error_size, "unknown C++ solid failure");
  }
  return 1;
}

// The smallest distance between a (posed by ta) and b (posed by tb), capped
// at search: the kernel only looks that far, and returns search when the
// solids are further apart. 0 when they touch or overlap.
// Stops early, with some distance at most `enough`, once the solids are
// known to be that close.
// With inset > 0 the moving solid is first shrunk by that much (see Tree):
// a gap of 0 then means the two really overlap, not that they touch.
extern "C" int coscad_solid_gap(const void *a, const double *ta, const void *b, const double *tb,
                                double search, double enough, double inset, double *gap, char *error, size_t error_size) noexcept {
  if (!a || !b || !ta || !tb || !gap || (error_size && !error)) return 1;
  if (error_size) error[0] = '\0';
  try {
    // a moves rigidly relative to b: express a's pose in b's frame
    const auto &sa = *static_cast<const Solid*>(a), &sb = *static_cast<const Solid*>(b);
    if (!identity_pose(tb)) throw std::runtime_error("gap: the obstacle must be posed already");
    double r[3][3]; for (int i = 0; i < 3; ++i) for (int j = 0; j < 3; ++j) r[i][j] = ta[4*i+j];
    *gap = tree_gap(inset > 0 ? sa.inset(inset) : sa.get(), r, V{ta[3], ta[7], ta[11]}, sb.get(), search, enough);
    return 0;
  } catch (const std::exception &e) {
    if (error_size) std::snprintf(error, error_size, "%s", e.what());
  } catch (...) {
    if (error_size) std::snprintf(error, error_size, "unknown C++ gap failure");
  }
  return 1;
}

// The volume of a ∩ b, and when want_points is set the intersection's
// vertices (for a contact patch). The caller frees the points with
// coscad_mesh_free.
extern "C" int coscad_solid_overlap(const void *a, const double *ta, const void *b, const double *tb,
                                    int want_points, double *volume, double **points, size_t *np,
                                    char *error, size_t error_size) noexcept {
  if (!a || !b || !ta || !tb || !volume || !points || !np || (error_size && !error)) return 1;
  *points = nullptr; *np = 0;
  if (error_size) error[0] = '\0';
  try {
    const auto &sa = static_cast<const Solid*>(a)->m, &sb = static_cast<const Solid*>(b)->m;
    const Manifold both = posed(sa, ta) ^ posed(sb, tb);
    if (both.Status() != Manifold::Error::NoError)
      throw std::runtime_error(std::string("overlap: ")+status_name(both.Status()));
    *volume = both.Volume();
    if (want_points && !both.IsEmpty()) {
      const auto mesh = both.GetMeshGL64();
      const size_t n = mesh.NumVert();
      if (n > 3*max_triangles) throw std::runtime_error("overlap exceeds mesh limits");
      auto buffer = std::make_unique<double[]>(3*n);
      for (size_t i = 0; i < n; ++i)
        for (size_t d = 0; d < 3; ++d) buffer[3*i+d] = mesh.vertProperties[mesh.numProp*i+d];
      *np = n;
      *points = buffer.release();
    }
    return 0;
  } catch (const std::exception &e) {
    if (error_size) std::snprintf(error, error_size, "%s", e.what());
  } catch (...) {
    if (error_size) std::snprintf(error, error_size, "unknown C++ overlap failure");
  }
  return 1;
}
