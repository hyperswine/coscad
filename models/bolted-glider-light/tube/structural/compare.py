"""Hex8 linear elasticity of sliced deposited volume; lengths mm, forces N."""
import json, math, os, re, sys
from pathlib import Path
import numpy as np
from scipy.sparse import coo_matrix
from scipy.sparse.linalg import spsolve

def deposition(name,h):
    folder=Path(os.environ.get('GLIDER_FEA_DIR','/tmp'))
    offsets=json.loads((folder/f'glider-{name}-offsets').read_text())
    lines=(folder/f'glider-{name}-body.gcode').read_text().splitlines()
    active=False
    x=y=z=0.
    cells={}
    for line in lines:
        if line.startswith('; OBJECT_ID:'): active=True
        if line.startswith('; FEATURE: Custom'): active=False
        if not line.startswith(('G0 ','G1 ','G2 ','G3 ')): continue
        f={k:float(v) for k,v in re.findall(r'([XYZE])(-?[\d.]+)',line.split(';')[0])}
        xx,yy,zz=f.get('X',x),f.get('Y',y),f.get('Z',z)
        if active and f.get('E',0)>0 and (xx!=x or yy!=y):
            n=max(1,math.ceil(math.hypot(xx-x,yy-y)/(h*.33)))
            volume=f['E']*math.pi*.875**2/n
            for i in range(n):
                t=(i+.5)/n
                a=x+t*(xx-x)-128+offsets[0]
                b=y+t*(yy-y)-128+offsets[1]
                key=(math.floor((a+b)/math.sqrt(2)/h),math.floor((b-a)/math.sqrt(2)/h),math.floor((zz-.1)/h))
                cells[key]=cells.get(key,0)+volume
        x,y,z=xx,yy,zz
    return cells

def solve(name,h):
    cells=deposition(name,h)
    signs=np.array([(a,b,c) for c in [-1,1] for b in [-1,1] for a in [-1,1]])
    corners=(signs+1)//2
    nodes,inv=np.unique((np.array(list(cells))[:,None,:]+corners).reshape(-1,3),axis=0,return_inverse=True)
    en=inv.reshape(-1,8); xyz=nodes*h
    dof=(en[:,:,None]*3+np.arange(3)).reshape(-1,24)
    E=2500.; nu=.35
    G=E/(2*(1+nu)); lam=2*G*nu/(1-2*nu)
    D=np.diag([2*G]*3+[G]*3); D[:3,:3]+=lam
    def strain(x):
        grad=np.array([signs[:,i]*np.prod(1+signs[:,[j for j in range(3) if j!=i]]*np.delete(x,i),axis=1)/(4*h) for i in range(3)])
        B=np.zeros((6,24))
        for row,(i,j) in enumerate([(0,0),(1,1),(2,2),(0,1),(1,2),(0,2)]):
            B[row,i::3]=grad[j]
            if i!=j:B[row,j::3]=grad[i]
        return B
    K=sum((B.T@D@B)*(h/2)**3 for B in [strain(s/math.sqrt(3)) for s in signs])
    frac=np.minimum(1,np.array(list(cells.values()))/h**3)
    M=coo_matrix(((frac[:,None,None]*K).ravel(),(np.repeat(dof,24,axis=1).ravel(),np.tile(dof,(1,24)).ravel())),shape=(3*len(nodes),)*2).tocsc()
    x,y,z=xyz.T
    fixed=np.repeat((y>=165)&(y<=175),3)
    F=np.zeros((len(nodes),3))
    patches=[]
    for station in [26.4,74.4]:
        patch=(x>=9)&(x<=21)&(abs(y-station)<=5)&(abs(z-(44+.105*x-.0437*y))<=h*.75)
        assert sum(patch)>0
        F[patch,2]+=.5/sum(patch)
        patches.append(patch)
    u=np.zeros(F.size); u[~fixed]=spsolve(M[~fixed][:,~fixed],F.ravel()[~fixed])
    stress=u[dof]@strain(np.zeros(3)).T@D
    a,b,c,d,e,f=stress.T
    vm=np.sqrt(((a-b)**2+(b-c)**2+(c-a)**2)/2+3*(d*d+e*e+f*f))
    centres=(np.array(list(cells))+.5)*h
    zones=[(abs(centres[:,1]-k)<11)&(centres[:,2]>20) for k in [26.4,74.4]]
    return dict(mesh_mm=h,n_elements=len(cells),pad_travel_mm=[float(np.mean(u.reshape(-1,3)[p,2])) for p in patches],pad_peak_MPa=[float(vm[p].max()) for p in zones],body_peak_MPa=float(vm[(centres[:,1]>0)&(centres[:,1]<160)].max()))
if __name__=='__main__':
    print(json.dumps(solve(sys.argv[1],float(sys.argv[2]))))
