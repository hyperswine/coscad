#!/usr/bin/env python3
"""Generate the 200 mm-grid tank concept from existing CoScad parts. Units mm.
No printer commands. The gaskets/seals are requirements, not invented solids.
"""
import hashlib,json,math,warnings
from pathlib import Path
import numpy as np
from scipy.spatial.transform import Rotation
ROOT=Path(__file__).resolve().parents[2]
OUT=Path(__file__).resolve().parent
P=OUT/'parts'

def sha(p): return hashlib.sha256(p.read_bytes()).hexdigest()
def rot(a): return Rotation.from_euler('xyz',a,degrees=True).as_matrix()
def euler(r):
    with warnings.catch_warnings():
        warnings.simplefilter('ignore',UserWarning)
        return Rotation.from_matrix(r).as_euler('xyz',degrees=True).round(9).tolist()
def main():
    P.mkdir(exist_ok=True)
    sources=['examples-next/panel_center.coscad','examples-next/bracket_seam.coscad','examples-next/tnut_m5.coscad','examples/assemble/plan/flat90.coscad','examples-next/rail_x180.coscad','examples-next/corner_90.coscad']
    parts={}; instances=[]; panels=[]; beams=[]
    def part(name,text,material,origin):
        (P/(name+'.coscad')).write_text('// Tank study derivative: '+origin+'\n'+text+'\n')
        parts[name]={'file':'parts/'+name+'.coscad','material':material,'source':origin}
    def place(name,kind,pos,r=np.eye(3),parent=None):
        ident=f'{kind}_{len(instances)+1:04}'
        instances.append({'id':ident,'part':name,'kind':kind,'position_mm':np.asarray(pos).round(9).tolist(),'rotation_xyz_deg':euler(r),'parent':parent})
        return ident
    rail=(ROOT/sources[4]).read_text().split('// Oriented joint interfaces')[0]
    for length in (180,980,1060):
        part(f'rail_{length}',rail.replace('180',str(length)).replace('182',str(length+2)),'aluminium',sources[4])
    for name,path,mat in [('seam_bracket',sources[1],'petg'),('tnut',sources[2],'steel'),('flat_connector',sources[3],'petg')]:
        part(name,(ROOT/path).read_text().split('// Oriented joint interfaces')[0].split('// Through-hole centres')[0],mat,path)
    part('m3_screw_preview','main = zcyl 1.5 8 ⊕ (ζ 5 (zcyl 2.8 2))','steel','schematic M3 shank/head; threads omitted')
    part('m5_screw_preview','main = xcyl 2.5 10 ⊕ (χ 6 (xcyl 4.25 3))','steel','schematic M5 shank/head; threads omitted')
    part('inside_corner',(ROOT/sources[5]).read_text().split('// Oriented joint interfaces')[0],'aluminium',sources[5])
    # Physical rail endpoints use 180 mm cut lengths; FE nodes are joint centres.
    seen=set()
    def beam(a,b,partname,centre,rotation):
        key=tuple(sorted([tuple(a),tuple(b)]))
        if key in seen:return
        seen.add(key); ident=place(partname,'rail',centre,rotation)
        beams.append({'id':ident,'a':list(a),'b':list(b)})
    # Posts are shared between adjoining wall frames.
    grid=list(range(-500,501,200))
    for x in grid:
        for y in grid:
            if abs(x)==500 or abs(y)==500:
                place('rail_1060','post',[x,y,500],rot([0,-90,0]))
                for z in range(0,1000,200):
                    beams.append({'id':f'post_{x}_{y}_{z}','a':[x,y,z],'b':[x,y,z+200]})
                beams.append({'id':f'post_extension_{x}_{y}','a':[x,y,1000],'b':[x,y,1030]})
    faces=[('back',[0,490,0],[90,0,0]),('front',[0,-490,0],[90,0,180]),('right',[490,0,0],[90,0,-90]),('left',[-490,0,0],[90,0,90]),('floor',[0,0,10],[0,0,0])]
    def panel_source(bounds):
        x0,x1,y0,y1=bounds
        # Preserve four existing hole centres at +/-80, thickness reduced 2.5 -> 2.
        return f'''// Existing panel_center construction, changed thickness and perimeter trim.
p = ◙ {x1-x0} {y1-y0} 2 0.8 |> move {(x0+x1)/2} {(y0+y1)/2} 0
h = zcyl 1.7 8
main = p |> cut (χ -80 h) |> cut (χ 80 h) |> cut (ψ -80 h) |> cut (ψ 80 h)'''
    for face,origin,angles in faces:
        R=rot(angles); O=np.array(origin,dtype=float)
        world=lambda p:O+R@np.asarray(p)
        if face!='floor':
            for u in range(-400,401,200):
                for v in range(0,1001,200):
                    a=world([u-100,v,-10]); b=world([u+100,v,-10])
                    beam(a.round(6),b.round(6),'rail_180',world([u,v,-10]),R)
                    # Two L plates per rail join, on the dry outside of the rail.
                    for sign in (-1,1):
                        end=u+sign*100
                        up=1 if sign==-1 else -1
                        # local plate XY -> wall UV; hole pairs on post and crossrail.
                        rr=rot([0,0,0 if sign==-1 else 90])
                        if up==-1:rr=rot([180,0,0])@rr
                        # These connectors are displayed separately from panel mounts.
                        connector_centre=np.array([end-sign*10,v+up*10,-23.])
                        connector=place('flat_connector','frame_connector',world(connector_centre),R@rr)
                        for hole in ([10,-10,0],[-10,10,0]):
                            hp=connector_centre+rr@hole
                            place('m5_screw_preview','m5_preview',world([hp[0],hp[1],-22]),R@rot([0,90,0]),connector)
                            place('tnut','tnut',world([hp[0],hp[1],-17.25]),R@rot([0,90,0]),connector)
        for i,u in enumerate(range(-400,401,200)):
            for j,v in enumerate(range(-400,401,200) if face=='floor' else range(100,1000,200)):
                x0=-88 if i==0 else -100; x1=88 if i==4 else 100
                y0=(-88 if j==0 else -100) if face=='floor' else (-88 if j==0 else -100)
                y1=(88 if j==4 else 100) if face=='floor' else (112 if j==4 else 100)
                bounds=[x0,x1,y0,y1]
                name='panel_'+('_'.join(str(int(n)).replace('-','m') for n in bounds))
                if name not in parts:part(name,panel_source(bounds),'pla',sources[0])
                centre=world([u,v,1]); ident=place(name,'panel',centre,R)
                record={'id':ident,'face':face,'u':u,'v':v,'origin_mm':world([u,v,0]).tolist(),'rotation':R.tolist(),'bounds_local_mm':bounds,'thickness_mm':2,'supports':[]}
                # Source bracket pilot is x=7.5; backing plate at x=-2.5.
                # Keep the original support/hole pattern 20 mm from grid centreline.
                for du,dv,angle in [(-100,0,0),(100,0,180),(0,-100,90),(0,100,-90)]:
                    q=rot([0,0,angle]); direction=q[:,0]
                    local=np.array([u+du,v+dv,-10])+12.5*direction
                    br=place('seam_bracket','panel_bracket',world(local),R@q,ident)
                    # T-nut neck stops before bracket key; schematic fasteners excluded from interference verdicts.
                    place('tnut','tnut',world(local-5.1*direction),R@q,br)
                    place('m5_screw_preview','m5_preview',world(local+0.5*direction),R@q,br)
                    hole=np.array([u+du,v+dv,0])+20*direction
                    place('m3_screw_preview','m3_preview',world(hole+[0,0,-2]),R,br)
                    record['supports'].append({'local_centre_mm':(hole[:2]-[u,v]).tolist(),'attachment_mm':world([u+du,v+dv,-10]).round(6).tolist(),'bearing_point_mm':world(hole).tolist(),'bracket':br})
                panels.append(record)
    # Interior bottom grid: continuous Y rails, X pieces butted between them.
    # Split at every crossing in FE; rail geometry is kept as actual cut lengths.
    for x in range(-300,301,200):
        place('rail_980','rail',[x,0,0],rot([0,0,90]))
        for y in range(-500,500,200):beams.append({'id':f'floor_y_{x}_{y}','a':[x,y,0],'b':[x,y+200,0]})
    for y in range(-300,301,200):
        for x in range(-400,401,200):beam([x-100,y,0],[x+100,y,0],'rail_180',[x,y,0],np.eye(3))
    # Existing inside-corner connectors close the bottom grid joints.
    def floor_corner(node,Q):
        node=np.asarray(node,float);C=node+Q@np.array([18.5,12.,0.]);R=Q@rot([0,0,180])
        ident=place('inside_corner','floor_connector',C,R)
        # Source hole centres are (-1.5,0,0) along Y and (6.5,-8,0) along X.
        for hp,axis in [([-1.5,0,0],np.array([0.,1.,0.])),([6.5,-8,0],np.array([1.,0.,0.]))]:
            centre=C+R@hp;direction=R@axis
            # Purchased screw/T-nut previews; no thread/contact qualification.
            B=rot([0,0,90]) if axis[1] else np.eye(3)
            place('m5_screw_preview','m5_preview',centre,R@B,ident)
            place('tnut','tnut',centre+direction*4.75,R@B,ident)
    for y in range(-300,301,200):
        for x in range(-400,401,200):
            floor_corner([x-100,y,0],np.eye(3))
            floor_corner([x+100,y,0],rot([0,180,0]))
    for x in range(-300,301,200):
        floor_corner([x,-500,0],rot([0,0,90]))
        floor_corner([x,500,0],rot([0,0,-90]))
    # A reviewable design assembly; hardware preview is not thread/contact geometry.
    lines=['// Water-tank concept. No seal design or build approval.','plate 250 250 6','clearance 0']
    for obj in instances:
        qty=1 if obj['kind'] in ('panel','panel_bracket','frame_connector') else 0
        lines.append(f"{obj['id']} ← {parts[obj['part']]['file']} ×{qty} material={parts[obj['part']]['material']}")
    for obj in instances:
        pose=obj['position_mm']+obj['rotation_xyz_deg']
        lines.append('fixed '+obj['id']+' '+' '.join(f'{x:.9g}' for x in pose))
    (OUT/'tank.assemble').write_text('\n'.join(lines)+'\n')
    data={'schema':'coscad.water-tank-concept.v1','water_inside_mm':[976,976,1000],'water_bottom_z_mm':12,'water_heights_mm':[250,500,750,1000],
      'panel_material':'PLA, 2 mm, illustrative isotropic E=2300 MPa, nu=0.35','bracket_material':'Existing PETG, illustrative E=2000 MPa, nu=0.3',
      'frame_material':'Aluminium E=69000 MPa, nu=0.33; exact alloy not supplied','seals':'Required at every seam/hole/corner; unspecified, no structural stiffness credited',
      'source_sha256':{p:sha(ROOT/p) for p in sources},'parts':parts,'instances':instances,'panels':panels,'beams':beams,
      'limitations':['Prototype arrangement, not a qualified tank','Fastener solids are schematic; purchased T-nut model is already a crude preview','Rigid frame FE joints are an explicit optimistic assumption','Base requires continuous rigid support/anchorage for the FE boundary condition','Seals, joint slip, creep and contact are not qualified']}
    (OUT/'model.json').write_text(json.dumps(data,indent=2)+'\n')
    from collections import Counter
    (OUT/'bom.json').write_text(json.dumps({'counts':dict(Counter(x['kind'] for x in instances)),'part_counts':dict(Counter(x['part'] for x in instances)),'note':'Panel, wall-frame and floor-grid hardware is included schematically. Seal details, purchased hardware fit/preload and base anchorage remain unqualified; no released cutting/build BOM.'},indent=2)+'\n')
    # Deterministic focused geometry fixtures. Threads/preload are excluded.
    central=next(p for p in panels if p['face']=='back' and p['u']==0 and p['v']==500)
    unit=[x for x in instances if x['id']==central['id'] or (x['kind']=='panel_bracket' and x['parent']==central['id']) or (x['kind']=='post' and x['position_mm'][0] in (-100,100) and x['position_mm'][1]==500) or (x['kind']=='rail' and x['position_mm'][0]==0 and x['position_mm'][1]==500 and x['position_mm'][2] in (400,600))]
    corner=[x for x in instances if (x['kind']=='post' and x['position_mm'][:2]==[500.,500.]) or (x['kind']=='rail' and x['position_mm'] in ([400.,500.,400.],[500.,400.,400.])) or (x['kind']=='frame_connector' and x['position_mm'] in ([490.,513.,390.],[513.,490.,410.]))]
    floor_ids={p['id'] for p in panels if (p['face']=='floor' and p['u']==400 and p['v']==400) or (p['face']=='back' and p['u']==400 and p['v']==100) or (p['face']=='right' and p['u']==-400 and p['v']==100)}
    floorcorner=[x for x in instances if x['id'] in floor_ids or (x['kind']=='panel_bracket' and x['parent'] in floor_ids) or (x['kind']=='floor_connector' and x['position_mm'][0]>=280 and x['position_mm'][1]>=280) or (x['kind']=='post' and x['position_mm'][0]>=300 and x['position_mm'][1]>=300) or (x['kind']=='rail' and x['position_mm'] in ([400.,500.,0.],[500.,400.,0.],[400.,300.,0.],[300.,0.,0.]))]
    for name,objects in [('unit-bay',unit),('corner-joint',corner),('floor-corner',floorcorner)]:
        lines=['// Focused geometry check. Schematic threaded hardware excluded.','plate 250 250 6','clearance 0']
        for x in objects:lines.append(f"{x['id']} ← {parts[x['part']]['file']} ×0")
        for x in objects:lines.append('fixed '+x['id']+' '+' '.join(f'{v:.9g}' for v in x['position_mm']+x['rotation_xyz_deg']))
        (OUT/(name+'.assemble')).write_text('\n'.join(lines)+'\n')
    print(len(panels),'panels;',len(beams),'FE beam segments;',len(instances),'displayed instances')
if __name__=='__main__':main()
