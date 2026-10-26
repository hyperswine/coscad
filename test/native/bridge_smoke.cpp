// Boundary ownership/error tests. Compile the shim with ASan/UBSan; an ordinary
// prebuilt Manifold archive is not itself instrumented by this test.
#include <manifold/manifold.h>
#include <manifold/mesh.h>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <limits>
#include <vector>
extern "C" int coscad_mesh_op(int, const double *, size_t, const double *, size_t,
                              double **, size_t *, char *, size_t) noexcept;
extern "C" int coscad_mesh_extrude(const double *,size_t,double,double **,size_t *,char *,size_t) noexcept;
extern "C" void coscad_mesh_free(double *) noexcept;
extern "C" void coscad_mesh_free_indices(uint64_t *) noexcept;
extern "C" int coscad_mesh_op_indexed_timed(uint64_t,int,const double *,size_t,const uint64_t *,size_t,
    const double *,size_t,const uint64_t *,size_t,double **,size_t *,uint64_t **,size_t *,char *,size_t) noexcept;
extern "C" int coscad_mesh_op_indexed(int,const double *,size_t,const uint64_t *,size_t,
    const double *,size_t,const uint64_t *,size_t,double **,size_t *,uint64_t **,size_t *,char *,size_t) noexcept;

std::vector<double> soup(const manifold::Manifold &solid) {
  const auto mesh=solid.GetMeshGL64();
  std::vector<double> result;
  for (const auto v : mesh.triVerts)
    for (size_t d=0; d<3; ++d) result.push_back(mesh.vertProperties[mesh.numProp*v+d]);
  return result;
}

int main() {
  auto a=soup(manifold::Manifold::Cube({2,3,4}));
  auto b=soup(manifold::Manifold::Cube({2,3,4}).Translate({1,0,0}));
  char error[256];
  for (int repeat=0; repeat<100; ++repeat) {
    for (int op=0; op<5; ++op) {
      double *out=nullptr;
      size_t count=0;
      assert(coscad_mesh_op(op,a.data(),a.size()/9,b.data(),b.size()/9,&out,&count,error,sizeof error)==0);
      assert(out && count>0 && count<500000);
      for (size_t i=0; i<9*count; ++i) assert(std::isfinite(out[i]));
      coscad_mesh_free(out);
    }
  }
  double *out=nullptr;
  size_t count=0;
  assert(coscad_mesh_op(1,a.data(),a.size()/9,a.data(),a.size()/9,&out,&count,error,sizeof error)==0);
  assert(count==0);
  coscad_mesh_free(out);
  auto failure=[&](int op,const double *data,size_t n) {
    out=nullptr; count=99;
    assert(coscad_mesh_op(op,data,n,nullptr,0,&out,&count,error,sizeof error)!=0);
    assert(out==nullptr && count==0 && error[0]);
  };
  failure(99,a.data(),a.size()/9);
  failure(0,nullptr,1);
  failure(0,a.data(),500001);
  failure(0,a.data(),1); // one open triangle
  a[0]=std::numeric_limits<double>::quiet_NaN();
  failure(0,a.data(),a.size()/9);
  assert(coscad_mesh_op(0,nullptr,0,nullptr,0,nullptr,&count,error,sizeof error)!=0);
  assert(coscad_mesh_op(0,nullptr,0,nullptr,0,&out,nullptr,error,sizeof error)!=0);
  assert(coscad_mesh_op(0,nullptr,0,nullptr,0,&out,&count,nullptr,256)!=0);
  const double profile[]={0,0,4,0,4,1,1,1,1,4,0,4};
  for (int i=0; i<100; ++i) {
    assert(coscad_mesh_extrude(profile,6,2,&out,&count,error,sizeof error)==0);
    assert(out && count>0); coscad_mesh_free(out);
  }
  assert(coscad_mesh_extrude(profile,2,2,&out,&count,error,sizeof error)!=0 && out==nullptr && count==0);
  assert(coscad_mesh_extrude(profile,4097,2,&out,&count,error,sizeof error)!=0 && out==nullptr && count==0);
  assert(coscad_mesh_extrude(nullptr,3,2,&out,&count,error,sizeof error)!=0 && out==nullptr && count==0);
  assert(coscad_mesh_extrude(profile,6,-1,&out,&count,error,sizeof error)!=0 && out==nullptr && count==0);
  const double crossing[]={0,0,4,0,0,3,4,4};
  assert(coscad_mesh_extrude(crossing,4,2,&out,&count,error,sizeof error)!=0 && out==nullptr && count==0);
  const auto ia=manifold::Manifold::Cube({2,3,4}).GetMeshGL64();
  const auto ib=manifold::Manifold::Cube({2,3,4}).Translate({1,0,0}).GetMeshGL64();
  double *vertices=nullptr; uint64_t *faces=nullptr; size_t nv=0,nt=0;
  for (int repeat=0; repeat<100; ++repeat) for (int op=0; op<5; ++op) {
    assert(coscad_mesh_op_indexed(op,ia.vertProperties.data(),ia.NumVert(),ia.triVerts.data(),ia.NumTri(),
      ib.vertProperties.data(),ib.NumVert(),ib.triVerts.data(),ib.NumTri(),&vertices,&nv,&faces,&nt,error,sizeof error)==0);
    assert(nv>0 && nt>0);
    for (size_t i=0;i<3*nv;++i) assert(std::isfinite(vertices[i]));
    for (size_t i=0;i<3*nt;++i) assert(faces[i]<nv);
    coscad_mesh_free(vertices); coscad_mesh_free_indices(faces);
  }
  auto indexed_failure=[&](const double *v,size_t n,const uint64_t *f,size_t t) {
    vertices=nullptr; faces=nullptr; nv=99; nt=99;
    assert(coscad_mesh_op_indexed(0,v,n,f,t,nullptr,0,nullptr,0,&vertices,&nv,&faces,&nt,error,sizeof error)!=0);
    assert(!vertices && !faces && nv==0 && nt==0 && error[0]);
  };
  auto bad_indices=ia.triVerts; bad_indices[0]=std::numeric_limits<uint64_t>::max();
  indexed_failure(ia.vertProperties.data(),ia.NumVert(),bad_indices.data(),ia.NumTri());
  indexed_failure(nullptr,1,nullptr,0);
  indexed_failure(ia.vertProperties.data(),ia.NumVert(),nullptr,1);
  indexed_failure(ia.vertProperties.data(),1500001,ia.triVerts.data(),ia.NumTri());
  indexed_failure(ia.vertProperties.data(),ia.NumVert(),ia.triVerts.data(),500001);
  auto bad_vertices=ia.vertProperties; bad_vertices[0]=std::numeric_limits<double>::infinity();
  indexed_failure(bad_vertices.data(),ia.NumVert(),ia.triVerts.data(),ia.NumTri());
  assert(coscad_mesh_op_indexed(1,ia.vertProperties.data(),ia.NumVert(),ia.triVerts.data(),ia.NumTri(),
      ia.vertProperties.data(),ia.NumVert(),ia.triVerts.data(),ia.NumTri(),&vertices,&nv,&faces,&nt,error,sizeof error)==0);
  assert(nv==0 && nt==0); coscad_mesh_free(vertices); coscad_mesh_free_indices(faces);
  assert(coscad_mesh_op_indexed(0,nullptr,0,nullptr,0,nullptr,0,nullptr,0,nullptr,&nv,&faces,&nt,error,sizeof error)!=0);
  assert(coscad_mesh_op_indexed(0,nullptr,0,nullptr,0,nullptr,0,nullptr,0,&vertices,&nv,nullptr,&nt,error,sizeof error)!=0);
  assert(coscad_mesh_op_indexed(0,nullptr,0,nullptr,0,nullptr,0,nullptr,0,&vertices,&nv,&faces,&nt,nullptr,256)!=0);
  // A near-coincident curved difference must either survive an indexed
  // round trip without measurable volume drift or be explicitly refused.
  const auto sphere=manifold::Manifold::Sphere(3,50).GetMeshGL64();
  auto rounded=sphere;
  for (auto &value:rounded.vertProperties) value=static_cast<float>(value);
  const int first=coscad_mesh_op_indexed(1,sphere.vertProperties.data(),sphere.NumVert(),sphere.triVerts.data(),sphere.NumTri(),
      rounded.vertProperties.data(),rounded.NumVert(),rounded.triVerts.data(),rounded.NumTri(),&vertices,&nv,&faces,&nt,error,sizeof error);
  if (first==0) {
    auto volume=[](const double *v,const uint64_t *f,size_t count) {
      double sum=0;
      for(size_t i=0;i<count;++i){const auto a=v+3*f[3*i],b=v+3*f[3*i+1],c=v+3*f[3*i+2];
        sum+=(a[0]*(b[1]*c[2]-b[2]*c[1])+a[1]*(b[2]*c[0]-b[0]*c[2])+a[2]*(b[0]*c[1]-b[1]*c[0]))/6;}
      return sum;
    };
    const auto before=volume(vertices,faces,nt);
    double *next_vertices=nullptr; uint64_t *next_faces=nullptr; size_t next_nv=0,next_nt=0;
    const int next=coscad_mesh_op_indexed(0,vertices,nv,faces,nt,nullptr,0,nullptr,0,
      &next_vertices,&next_nv,&next_faces,&next_nt,error,sizeof error);
    if (next==0) assert(std::abs(volume(next_vertices,next_faces,next_nt)-before)<=std::max(1e-8,std::abs(before)*1e-10));
    else assert(!next_vertices && !next_faces && next_nv==0 && next_nt==0 && error[0]);
    coscad_mesh_free(next_vertices);coscad_mesh_free_indices(next_faces);
    coscad_mesh_free(vertices);coscad_mesh_free_indices(faces);
  } else assert(!vertices && !faces && nv==0 && nt==0 && error[0]);
  // Real short-deadline cancellation during a nontrivial kernel operation.
  const auto dense=manifold::Manifold::Sphere(3,128).GetMeshGL64();
  assert(coscad_mesh_op_indexed_timed(1,4,dense.vertProperties.data(),dense.NumVert(),dense.triVerts.data(),dense.NumTri(),
      dense.vertProperties.data(),dense.NumVert(),dense.triVerts.data(),dense.NumTri(),&vertices,&nv,&faces,&nt,error,sizeof error)!=0);
  assert(!vertices && !faces && nv==0 && nt==0 && std::string(error).find("deadline exceeded")!=std::string::npos);
  // Cancellation must not poison a subsequent operation's fresh context.
  assert(coscad_mesh_op_indexed(0,ia.vertProperties.data(),ia.NumVert(),ia.triVerts.data(),ia.NumTri(),
      nullptr,0,nullptr,0,&vertices,&nv,&faces,&nt,error,sizeof error)==0);
  assert(nv>0 && nt>0); coscad_mesh_free(vertices); coscad_mesh_free_indices(faces);
  coscad_mesh_free_indices(nullptr);
  coscad_mesh_free(nullptr);
  std::puts("PASS: 500 indexed + 500 soup CSG operations, 100 extrusions, empty output, malformed input, null arguments, near-coincident volume guard, deadline cancellation/recovery and ownership");
}
