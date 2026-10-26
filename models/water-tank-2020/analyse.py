#!/usr/bin/env python3
"""Auditable hydrostatic component study: Morley plates + 3D beam frame.
Optional companion to CoScad's solid FEA, not a coupled contact/nonlinear solve.
Uses the pinned experiments/requirements-fea.txt environment, mm / N / MPa.
"""
import argparse, functools, hashlib, json, math
from pathlib import Path
import numpy as np
import scipy
from scipy.sparse import coo_matrix
from scipy.sparse.linalg import splu
import skfem
from skfem import Basis, MeshTri, ElementTriMorley, BilinearForm, LinearForm
from skfem.helpers import dd,ddot,trace
HERE=Path(__file__).resolve().parent
G=9.80665e-6  # rho=1000 kg/m3, g=9.80665 m/s2 -> pressure MPa per mm head
E=2300.; NU=.35; THICK=2.

def finite_json(path,data):
    def compact(value):
        if isinstance(value,dict):return {k:compact(v) for k,v in value.items() if k not in ('point_reactions','loads')}
        if isinstance(value,list):return [compact(v) for v in value]
        return value
    path.write_text(json.dumps(compact(data),indent=2,allow_nan=False)+'\n')

def patches(x,y):
    return [((abs(x+80)<=6+1e-8)&(abs(y)<=7.5+1e-8)),
            ((abs(x-80)<=6+1e-8)&(abs(y)<=7.5+1e-8)),
            ((abs(x)<=7.5+1e-8)&(abs(y+80)<=6+1e-8)),
            ((abs(x)<=7.5+1e-8)&(abs(y-80)<=6+1e-8))]

@functools.lru_cache(maxsize=48)
def plate_system(bounds,n,mode,waterlines=()):
    x0,x1,y0,y1=bounds
    axes=[]
    for a,b,extra in [(x0,x1,()),(y0,y1,waterlines)]:
        critical=[-90,-86,-80,-74,-7.5,0,7.5,74,80,86,90,*extra]
        axes.append(np.unique(np.round(np.r_[np.linspace(a,b,n+1),[v for v in critical if a<v<b]],10)))
    mesh=MeshTri.init_tensor(*axes)
    basis=Basis(mesh,ElementTriMorley(),intorder=4)
    D=E*THICK**3/(12*(1-NU**2))
    @BilinearForm
    def stiffness(u,v,w):return D*((1-NU)*ddot(dd(u),dd(v))+NU*trace(dd(u))*trace(dd(v)))
    K=stiffness.assemble(basis).tocsc()
    groups=[basis.nodal_dofs[0,mask] for mask in patches(mesh.p[0],mesh.p[1])]
    if mode=='rail_and_tabs':
        x,y=mesh.p;claimed=np.zeros(mesh.nvertices,dtype=bool);groups=[]
        pm=patches(x,y)
        for edge,line in enumerate([abs(x+90)<1e-7,abs(x-90)<1e-7,abs(y+90)<1e-7,abs(y-90)<1e-7]):
            mask=(pm[edge]|line)&~claimed;claimed|=mask;groups.append(basis.nodal_dofs[0,mask])
        fixed=np.unique(np.concatenate(groups))
    elif mode=='tabs':fixed=np.unique(np.concatenate(groups))
    elif mode=='simply_supported':fixed=basis.get_dofs().all('u')
    elif mode=='clamped':fixed=basis.get_dofs().all()
    else:raise ValueError('unknown support')
    if not len(fixed):raise ValueError('missing support')
    free=np.setdiff1d(np.arange(basis.N),fixed)
    factor=splu(K[free][:,free])
    return mesh,basis,K,groups,fixed,free,factor

def plate(bounds,n,mode,head,origin_y=0.,floor=False,constant=None,field=False):
    waterlines=() if floor or constant is not None else tuple(h+12-origin_y for h in (250,500,750,1000))
    mesh,basis,K,groups,fixed,free,factor=plate_system(tuple(bounds),n,mode,waterlines)
    def pressure(y):
        if constant is not None:return np.zeros_like(y)+constant
        return np.zeros_like(y)+G*head if floor else G*np.maximum(head+12-origin_y-y,0)
    @LinearForm
    def load(v,w):return pressure(w.x[1])*v
    f=load.assemble(basis); u=np.zeros(basis.N);u[free]=factor.solve(f[free]);res=K@u-f
    total=float(f[basis.nodal_dofs[0]].sum())
    ratio=float(np.linalg.norm(res[free])/max(np.linalg.norm(f[free]),1e-20))
    if ratio>1e-7 or not np.all(np.isfinite(u)):raise ValueError('plate residual/nonfinite failure')
    # Recover surface bending stress from element Hessian. Peaks at supports are mesh-dependent.
    h=basis.interpolate(u).hess[:,:,:,0]
    sx=E*THICK/(2*(1-NU**2))*(h[0,0]+NU*h[1,1]);sy=E*THICK/(2*(1-NU**2))*(h[1,1]+NU*h[0,0])
    txy=E*THICK/(2*(1+NU))*h[0,1]
    vm=np.sqrt(np.maximum(sx*sx-sx*sy+sy*sy+3*txy*txy,0))
    # Nodal maximum; Morley interpolation at quadrature also checks interior extrema samples.
    wnod=u[basis.nodal_dofs[0]]; maxw=max(float(abs(wnod).max()),float(abs(basis.interpolate(u)).max()))
    support=[]
    for ids in groups:
        reaction=-res[ids]; coords=basis.doflocs[:,ids]
        support.append({'force_n':float(reaction.sum()),'first_moment_n_mm':(coords@reaction).tolist(),
                        'point_reactions':[{'xy_mm':xy.tolist(),'force_n':float(force)} for xy,force in zip(coords.T,reaction)]})
    force_balance=abs(float(res[basis.nodal_dofs[0]].sum())+total)
    xy=basis.global_coordinates()
    applied_first=np.array([np.sum(xy[d]*pressure(xy[1])*basis.dx) for d in (0,1)])
    moment_error=float(np.linalg.norm(sum((np.array(q['first_moment_n_mm']) for q in support),np.zeros(2))-applied_first)) if mode in ('tabs','rail_and_tabs') else None
    if moment_error is not None and moment_error>max(1e-5,total*1e-6):raise ValueError('plate support moment imbalance')
    order=np.argsort(vm);areas=basis.dx.sum(axis=1); cumulative=np.cumsum(areas[order]);p95=float(vm[order[np.searchsorted(cumulative,.95*cumulative[-1])]])
    if force_balance>max(1e-6,total*1e-7):raise ValueError('plate force imbalance')
    analytic=(bounds[1]-bounds[0])*((bounds[3]-bounds[2])*constant if constant is not None else (G*head*(bounds[3]-bounds[2]) if floor else G/2*(max(head+12-origin_y-bounds[2],0)**2-max(head+12-origin_y-bounds[3],0)**2)))
    if abs(total-analytic)>max(1e-7,abs(analytic)*1e-9):raise ValueError('hydrostatic load integration failed')
    result={'max_abs_displacement_mm':maxw,'max_surface_bending_von_mises_mpa':float(vm.max()),'stress_p95_mpa':p95,
      'load_n':total,'residual_relative':ratio,'force_balance_n':force_balance,'moment_balance_n_mm':moment_error,'supports':support,'elements':mesh.t.shape[1],
      'w_over_thickness':maxw/THICK,'linear_geometry_credible':maxw/THICK<=.5,'support_mode':mode,'cells_per_side':n,
      'scope':'Linear Kirchhoff plate, no holes/rounding, finite tabs and optional rail bearing lines, or ideal continuous edges. Support peaks not qualified.'}
    if field:result['field']={'xy_mm':mesh.p.T.tolist(),'triangles':mesh.t.T.tolist(),'w_mm':wnod.tolist(),'element_vm_mpa':vm.tolist()}
    return result

def section_from_stl(path,length=180.):
    vertices=[]
    for line in path.read_text().splitlines():
        if line.strip().startswith('vertex '):vertices.append([float(x) for x in line.split()[1:]])
    t=np.array(vertices).reshape(-1,3,3)
    vol=np.einsum('ij,ij->i',t[:,0],np.cross(t[:,1],t[:,2]))/6
    sign=1 if vol.sum()>0 else -1;vol*=sign
    def second(axis):
        q=t[:,:,axis];return float(np.sum(vol*(np.sum(q*q,axis=1)+q[:,0]*q[:,1]+q[:,0]*q[:,2]+q[:,1]*q[:,2])/10))
    A=float(vol.sum()/length);Iy=second(2)/length;Iz=second(1)/length
    if not (0<A<400 and Iy>0 and Iz>0):raise ValueError('invalid extrusion section')
    return {'area_mm2':A,'Iy_mm4':Iy,'Iz_mm4':Iz,'source_stl_sha256':hashlib.sha256(path.read_bytes()).hexdigest(),'method':'Signed tetrahedral volume/second moments of native STL divided by 180 mm; not a solid 20x20 bar','J_assumption':'0.1*(Iy+Iz); sensitivity at 0.05 and 0.5; no actual torsion constant supplied'}

def beam_k(a,b,section,jfactor=.1):
    axis=np.asarray(b)-a;L=float(np.linalg.norm(axis));x=axis/L
    ref=np.array([0.,0.,1.]) if abs(x[2])<.9 else np.array([0.,1.,0.])
    y=np.cross(ref,x);y/=np.linalg.norm(y);z=np.cross(x,y);R=np.array([x,y,z])
    T=np.zeros((12,12))
    for i in (0,3,6,9):T[i:i+3,i:i+3]=R
    k=np.zeros((12,12));A=section['area_mm2'];Iy=section['Iy_mm4'];Iz=section['Iz_mm4'];EA=69000*A;GJ=69000/(2*1.33)*jfactor*(Iy+Iz)
    for ids,c in [([0,6],EA/L),([3,9],GJ/L)]:k[np.ix_(ids,ids)]+=c*np.array([[1,-1],[-1,1]])
    bmat=np.array([[12,6*L,-12,6*L],[6*L,4*L*L,-6*L,2*L*L],[-12,-6*L,12,-6*L],[6*L,2*L*L,-6*L,4*L*L]])
    for ids,I,sgn in [([1,5,7,11],Iz,[1,1,1,1]),([2,4,8,10],Iy,[1,-1,1,-1])]:
        k[np.ix_(ids,ids)]+=69000*I/L**3*bmat*np.outer(sgn,sgn)
    return k,T,L

def point_load(L,xi,F,M):
    N=np.array([1-3*xi**2+2*xi**3,L*(xi-2*xi**2+xi**3),3*xi**2-2*xi**3,L*(-xi**2+xi**3)])
    dN=np.array([(-6*xi+6*xi**2)/L,1-4*xi+3*xi**2,(6*xi-6*xi**2)/L,-2*xi+3*xi**2])
    f=np.zeros(12);f[[0,6]]=F[0]*np.array([1-xi,xi]);f[[3,9]]=M[0]*np.array([1-xi,xi])
    f[[1,5,7,11]]+=F[1]*N+M[2]*dN
    f[[2,4,8,10]]+=(F[2]*N-M[1]*dN)*[1,-1,1,-1]
    return f

def frame(model,loads,section,subdiv=2,jfactor=.1):
    nodes=[];index={};members=[];original=set()
    def key(p):return tuple(np.round(p,7))
    def node(p):
        q=key(p)
        if q not in index:index[q]=len(nodes);nodes.append(np.array(p))
        return index[q]
    for beam in model['beams']:
        a=np.array(beam['a'],float);b=np.array(beam['b'],float);original.update([key(a),key(b)])
        for j in range(subdiv):
            p=a+(b-a)*j/subdiv;q=a+(b-a)*(j+1)/subdiv
            ia,ib=node(p),node(q);k,T,L=beam_k(p,q,section,jfactor)
            members.append({'ids':np.r_[np.arange(6*ia,6*ia+6),np.arange(6*ib,6*ib+6)],'k':k,'T':T,'L':L,'a':p,'b':q,'id':beam['id'],'f':np.zeros(12),'loads':[]})
    nd=6*len(nodes);rows=[];cols=[];values=[];force=np.zeros(nd)
    for m in members:
        ids=m['ids'];kg=m['T'].T@m['k']@m['T']
        rows.extend(np.repeat(ids,12));cols.extend(np.tile(ids,12));values.extend(kg.ravel())
    K=coo_matrix((values,(rows,cols)),shape=(nd,nd)).tocsc()
    lines={}
    for j,m in enumerate(members):
        axis=int(np.argmax(abs(m['b']-m['a'])));other=[i for i in range(3) if i!=axis]
        lines.setdefault((axis,tuple(np.round(m['a'][other],6))),[]).append(j)
    for load in loads:
        p=np.array(load['attachment_mm']);F=np.array(load['force_n']);M=np.array(load['moment_n_mm'])
        choices=[]
        for axis in range(3):
            other=[i for i in range(3) if i!=axis]
            for j in lines.get((axis,tuple(np.round(p[other],6))),[]):
                m=members[j];xi=float((p[axis]-m['a'][axis])/(m['b'][axis]-m['a'][axis]))
                if -1e-7<=xi<=1+1e-7:choices.append((j,max(0.,min(1.,xi))))
        if not choices:raise ValueError('unsupported attachment '+str(p))
        j,xi=choices[0];m=members[j];R=m['T'][:3,:3];fl=R@F;ml=R@M
        f=point_load(m['L'],xi,fl,ml);m['f']+=f;m['loads'].append((xi*m['L'],fl,ml));force[m['ids']]+=m['T'].T@f
    fixed=np.array([6*i+d for i,p in enumerate(nodes) if abs(p[2])<1e-8 and key(p) in original for d in range(6)],int)
    free=np.setdiff1d(np.arange(nd),fixed);u=np.zeros(nd);u[free]=splu(K[free][:,free]).solve(force[free]);r=K@u-force
    residual=float(np.linalg.norm(r[free])/max(np.linalg.norm(force[free]),1e-20))
    if residual>1e-7 or not np.all(np.isfinite(u)):raise ValueError('frame residual failure')
    displacement=u.reshape(-1,6);reaction=r.reshape(-1,6)
    resultant=reaction[:,:3].sum(axis=0)+np.array([x['force_n'] for x in loads]).sum(axis=0)
    moment=(np.cross(np.array(nodes),reaction[:,:3])+reaction[:,3:]).sum(axis=0)+sum((np.cross(x['attachment_mm'],x['force_n'])+x['moment_n_mm'] for x in loads),np.zeros(3))
    if np.linalg.norm(resultant)>1e-5 or np.linalg.norm(moment)>1e-3:raise ValueError('frame balance failure')
    stresses=[]
    for m in members:
        local=m['T']@u[m['ids']];q=m['k']@local-m['f'];maxstress=0.;maxmoment=0.
        for pos in sorted(set([0,m['L']]+[x[0] for x in m['loads']])):
            for side in (-1,1):
                F=q[:3].copy();M=q[3:6].copy()+np.cross([-pos,0,0],F)
                for xp,fp,mp in m['loads']:
                    if xp<pos+side*1e-8:F+=fp;M+=mp+np.cross([xp-pos,0,0],fp)
                stress=abs(F[0])/section['area_mm2']+10*abs(M[1])/section['Iy_mm4']+10*abs(M[2])/section['Iz_mm4']
                maxstress=max(maxstress,stress);maxmoment=max(maxmoment,float(np.linalg.norm(M[1:])))
        stresses.append({'id':m['id'],'normal_stress_envelope_mpa':maxstress,'bending_moment_n_mm':maxmoment,'a':m['a'].tolist(),'b':m['b'].tolist()})
    imax=int(np.argmax(np.linalg.norm(displacement[:,:3],axis=1)))
    return {'max_displacement_mm':float(np.linalg.norm(displacement[imax,:3])),'max_displacement_location_mm':nodes[imax].tolist(),
      'max_normal_stress_envelope_mpa':max(x['normal_stress_envelope_mpa'] for x in stresses),'residual_relative':residual,
      'force_balance_n':float(np.linalg.norm(resultant)),'moment_balance_n_mm':float(np.linalg.norm(moment)),'subdivisions':subdiv,'torsion_factor':jfactor,
      'nodes_mm':np.array(nodes).tolist(),'displacements_mm':displacement[:,:3].tolist(),'members':stresses,
      'scope':'Euler-Bernoulli 3D beam frame, rigid joints, all original base-grid nodes fixed; no connector slip/contact or panel membrane stiffness. Stress is an axial+bending envelope, not bolt/contact von Mises.'}

def analytical_checks(section):
    # Navier series for simply supported square, constant pressure.
    D=E*THICK**3/(12*(1-NU**2));a=200.;p=.001
    exact=sum(16*p*a**4/(D*math.pi**6)*math.sin(m*math.pi/2)*math.sin(n*math.pi/2)/(m*n*(m*m+n*n)**2) for m in range(1,100,2) for n in range(1,100,2))
    checks=[]
    for n in (12,24,48):
        result=plate([-100,100,-100,100],n,'simply_supported',0,constant=p)
        checks.append({'mesh':n,'w_mm':result['max_abs_displacement_mm'],'navier_w_mm':exact,'relative_error':abs(result['max_abs_displacement_mm']/exact-1)})
    if checks[-1]['relative_error']>.02:raise ValueError('Navier plate benchmark failed')
    k,T,L=beam_k(np.array([0.,0.,0.]),np.array([1000.,0.,0.]),section)
    f=np.zeros(12);f[8]=-1;u=np.zeros(12);u[6:]=np.linalg.solve(k[6:,6:],f[6:])
    theory=1000**3/(3*69000*section['Iy_mm4'])
    err=abs(u[8]/-theory-1)
    if err>1e-10:raise ValueError('beam benchmark failed')
    # Load interpolation must preserve force and moment, including eccentric moments.
    f=point_load(200,.37,np.array([1.,2.,3.]),np.array([4.,5.,6.]));total=f[:3]+f[6:9];moment=f[3:6]+f[9:12]+np.cross([200,0,0],f[6:9])
    if not np.allclose(total,[1,2,3]) or not np.allclose(moment,np.array([4,5,6])+np.cross([74,0,0],[1,2,3])):raise ValueError('beam load resultant mismatch')
    return {'passed':True,'plate_navier':checks,'beam_cantilever_relative_error':err,'point_load_force_and_moment_passed':True}

def main():
    parser=argparse.ArgumentParser();parser.add_argument('--rail-stl',type=Path,required=True);parser.add_argument('--cells',type=int,default=48);args=parser.parse_args()
    if args.cells not in (12,24,48):parser.error('cells must be 12, 24 or 48')
    model=json.loads((HERE/'model.json').read_text());section=section_from_stl(args.rail_stl)
    evidence=HERE/'evidence';evidence.mkdir(exist_ok=True)
    checks=analytical_checks(section);finite_json(evidence/'analytical-checks.json',checks)
    results=[];refinements=[]
    for H in model['water_heights_mm']:
        print('water height',H,flush=True);records=[];loads=[];cache={}
        for panel in model['panels']:
            floor=panel['face']=='floor';bounds=panel['bounds_local_mm'];key=(tuple(bounds),0 if floor else panel['v'],floor)
            if key not in cache:cache[key]=plate(bounds,args.cells,'rail_and_tabs',H,panel['v'],floor)
            result=cache[key];R=np.array(panel['rotation']);O=np.array(panel['origin_mm']);normal=-R[:,2]
            records.append({'id':panel['id'],'face':panel['face'],'row_v_mm':panel['v'],**result})
            for edge,(support,reactions) in enumerate(zip(panel['supports'],result['supports'])):
                tab_force=0.
                for point in reactions['point_reactions']:
                    xy=np.array(point['xy_mm']);F=point['force_n']
                    on_tab=bool(patches(xy[0],xy[1])[edge])
                    if on_tab:
                        a=np.array(support['attachment_mm']);tab_force+=F
                    else:
                        local=[-100,xy[1],-10] if edge==0 else [100,xy[1],-10] if edge==1 else [xy[0],-100,-10] if edge==2 else [xy[0],100,-10]
                        a=O+R@local
                    bearing=O+R@np.r_[xy,0.]
                    moment=np.cross(bearing-a,F*normal)
                    loads.append({'attachment_mm':a.tolist(),'force_n':(F*normal).tolist(),'moment_n_mm':moment.tolist(),'bracket':support['bracket'] if on_tab else None})
                reactions['bracket_force_n']=tab_force
        total_floor=sum(p['load_n'] for p in records if p['face']=='floor');expected=G*H*976**2
        if abs(total_floor-expected)>1e-5:raise ValueError('floor water weight mismatch')
        for side in ('front','back','left','right'):
            if abs(sum(p['load_n'] for p in records if p['face']==side)-G*976*H**2/2)>1e-5:raise ValueError('wall force mismatch')
        frames=[frame(model,loads,section,s) for s in (1,2,4)]
        change=abs(frames[-1]['max_displacement_mm']/frames[-2]['max_displacement_mm']-1)
        if change>.05:raise ValueError('frame displacement refinement failed')
        sensitivities=[frame(model,loads,section,4,j) for j in (.05,.5)]
        worstwall=max((p for p in records if p['face']!='floor'),key=lambda p:p['max_abs_displacement_mm'])
        worstfloor=max((p for p in records if p['face']=='floor'),key=lambda p:p['max_abs_displacement_mm'])
        fieldcases=[]
        for worst in (worstwall,worstfloor):
            p=next(p for p in model['panels'] if p['id']==worst['id']);floor=p['face']=='floor'
            fine=[plate(p['bounds_local_mm'],n,'rail_and_tabs',H,p['v'],floor,field=n==72) for n in (24,48,72)]
            delta=abs(fine[-1]['max_abs_displacement_mm']/fine[-2]['max_abs_displacement_mm']-1)
            if delta>.05:raise ValueError('panel displacement refinement failed')
            comparison={mode:plate(p['bounds_local_mm'],48,mode,H,p['v'],floor) for mode in ('tabs','simply_supported','clamped')}
            fieldcases.append({'panel':p['id'],'face':p['face'],'bounds_mm':p['bounds_local_mm'],'refinement':[ {k:v for k,v in row.items() if k!='field'} for row in fine],
                              'last_relative_change':delta,'ideal_edge_comparison':comparison,'field':fine[-1]['field']})
        out={'water_height_mm':H,'water_litres':976**2*H/1e6,'water_weight_n':expected,'base_pressure_kpa':G*H*1000,'one_wall_force_n':G*976*H**2/2,
             'panels':records,'panel_detail':fieldcases,'frame':frames[-1],
             'frame_refinement':[{'subdivisions':f['subdivisions'],'w_mm':f['max_displacement_mm'],'normal_stress_mpa':f['max_normal_stress_envelope_mpa']} for f in frames],
             'frame_refinement_last_relative_change':change,'frame_torsion_sensitivity':[{'factor':f['torsion_factor'],'w_mm':f['max_displacement_mm'],'normal_stress_mpa':f['max_normal_stress_envelope_mpa']} for f in sensitivities],
             'max_bracket_normal_load_n':max(abs(s['bracket_force_n']) for p in records for s in p['supports']),'min_signed_support_force_n':min(s['force_n'] for p in records for s in p['supports']),'loads':loads}
        finite_json(evidence/f'height-{H}.json',out);results.append(out)
    summary={'schema':'coscad.tank-hydrostatic-study.v1','versions':{'scikit_fem':skfem.__version__,'scipy':scipy.__version__,'numpy':np.__version__},'section':section,'checks':checks,
             'assumptions':{'water_density_kg_m3':1000,'g_m_s2':9.80665,'PLA_E_mpa':E,'PLA_poisson':NU,'PLA_thickness_mm':THICK,'water_inside_mm':model['water_inside_mm']},
             'cases':[{k:v for k,v in r.items() if k in ('water_height_mm','water_litres','water_weight_n','base_pressure_kpa','one_wall_force_n','max_bracket_normal_load_n','min_signed_support_force_n','frame_refinement','frame_torsion_sensitivity')}|{'worst_panels':[{k:v for k,v in d.items() if k!='field'} for d in r['panel_detail']]} for r in results],
             'source_sha256':{p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in [Path(__file__),HERE/'build_model.py',HERE/'model.json']},
             'scope':'Component linear FEA. No coupled nonlinear tank, watertight seal, joint contact, creep or structural safety qualification. Displacements with w/t>0.5 are linear diagnostics only.'}
    finite_json(evidence/'summary.json',summary);print('all hydrostatic, residual, benchmark and refinement checks passed',flush=True)
if __name__=='__main__':main()
