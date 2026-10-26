#!/usr/bin/env python3
"""Render saved numerical evidence and the generated CoScad concept. No solves."""
import csv,hashlib,html,json,os,re
from pathlib import Path
os.environ.setdefault('MPLCONFIGDIR','/private/tmp/coscad-tank-matplotlib')
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.tri as mtri
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
import numpy as np
from scipy.spatial.transform import Rotation
HERE=Path(__file__).resolve().parent;EVID=HERE/'evidence'
plt.rcParams.update({'font.size':10,'axes.spines.top':False,'axes.spines.right':False,'figure.facecolor':'#f6f8fb','axes.facecolor':'#ffffff'})

def compact(v):
    if isinstance(v,dict):return {k:compact(x) for k,x in v.items() if k not in ('point_reactions','loads')}
    if isinstance(v,list):return [compact(x) for x in v]
    return v

def main():
    model=json.loads((HERE/'model.json').read_text());summary=json.loads((EVID/'summary.json').read_text())
    cases=[json.loads((EVID/f'height-{h}.json').read_text()) for h in (250,500,750,1000)]
    a=json.loads((EVID/'bracket-unit-2.json').read_text());b=json.loads((EVID/'bracket-unit-1.5.json').read_text())
    delta=abs(b['load_average_displacement_mm']/a['load_average_displacement_mm']-1)
    assert delta<.05
    rows=[]
    for c in cases:
        wall,floor=[x['refinement'][-1] for x in c['panel_detail']];F=c['max_bracket_normal_load_n']
        rows.append({'water_mm':c['water_height_mm'],'litres':c['water_litres'],'bottom_kpa':c['base_pressure_kpa'],'wall_force_n':c['one_wall_force_n'],
          'wall_linear_mm':wall['max_abs_displacement_mm'],'floor_linear_mm':floor['max_abs_displacement_mm'],
          'wall_linear_peak_mpa':wall['max_surface_bending_von_mises_mpa'],'floor_linear_peak_mpa':floor['max_surface_bending_von_mises_mpa'],
          'wall_area_p95_mpa':wall['stress_p95_mpa'],'floor_area_p95_mpa':floor['stress_p95_mpa'],
          'frame_mm':c['frame']['max_displacement_mm'],'frame_axial_bending_mpa':c['frame']['max_normal_stress_envelope_mpa'],
          'max_tab_n':F,'bracket_unit_fixture_mm':F*b['load_average_displacement_mm'],'bracket_unit_fixture_peak_mpa':F*b['max_cell_centre_vm_mpa'],
          'm5_single_shear_nominal_mpa':F/(np.pi*5**2/4),'panel_linear_valid':False})
    with (HERE/'results.csv').open('w') as f:
        writer=csv.DictWriter(f,fieldnames=rows[0]);writer.writeheader();writer.writerows(rows)
    (EVID/'bracket-scaling.json').write_text(json.dumps({'unit_displacement_refinement_change':delta,'cases':[{k:v for k,v in r.items() if k.startswith(('water','max_tab','bracket','m5'))} for r in rows],
      'scope':'Scaled 1 N actual seam-bracket FE fixture. Rigid back clamp is optimistic. M5 value is nominal gross-shank single-shear arithmetic, NOT thread, preload, T-nut pull-out, bending or fastener FEA.'},indent=2)+'\n')
    # A geometrical cutaway from the same instance manifest; two walls removed.
    full=cases[-1];by_id={p['id']:p for p in full['panels']}
    fig=plt.figure(figsize=(10,9));ax=fig.add_subplot(111,projection='3d');ax.set_facecolor('#f6f8fb')
    shown=[]
    for obj in model['instances']:
        kind=obj['kind'];p=np.array(obj['position_mm']);R=Rotation.from_euler('xyz',obj['rotation_xyz_deg'],degrees=True).as_matrix()
        if kind in ('rail','post'):
            L=float(obj['part'].split('_')[1]);a=p-R[:,0]*L/2;bpt=p+R[:,0]*L/2;ax.plot(*np.array([a,bpt]).T,color='#334155',linewidth=2,alpha=.85);shown.append(obj)
        elif kind=='panel':
            spec=next(x for x in model['panels'] if x['id']==obj['id'])
            if spec['face'] not in ('back','left','floor'):continue
            x0,x1,y0,y1=spec['bounds_local_mm'];verts=np.array([[x0,y0,0],[x1,y0,0],[x1,y1,0],[x0,y1,0]])@R.T+p
            colour='#93c5fd' if spec['face']=='floor' else '#cbd5e1'
            ax.add_collection3d(Poly3DCollection([verts],facecolors=colour,edgecolors='#64748b',linewidths=.45,alpha=.8));shown.append(obj)
        elif kind in ('panel_bracket','frame_connector','floor_connector'):
            if p[1]>480 or p[0]<-480 or p[2]<25:
                ax.scatter(*p,s=5,color='#dd6b20',depthshade=False);shown.append(obj)
    ax.set(xlim=(-550,550),ylim=(-550,550),zlim=(-40,1070),xlabel='X (mm)',ylabel='Y (mm)',zlabel='Z (mm)');ax.set_box_aspect([1,1,1]);ax.view_init(25,-55)
    ax.set_title('2020 tank concept • two walls removed\n125 PLA panels, 200 mm grid; clear water space 976 × 976 × 1000 mm',pad=22)
    fig.text(.08,.025,'Orange: connectors / panel tabs. Seams, seals and base anchorage are not qualified.\nThis is a cutaway of the generated instance positions, not a deformation plot.',fontsize=10,color='#475569')
    fig.savefig(HERE/'tank-cutaway.png',dpi=170,bbox_inches='tight');plt.close(fig)
    # Actual CoScad cutaway source, with reused part definitions namespaced.
    definitions=[]
    for name in sorted({x['part'] for x in shown}):
        text=(HERE/model['parts'][name]['file']).read_text()
        names=re.findall(r'^([A-Za-z_]\w*)\s*=',text,re.M)
        for symbol in sorted(names,key=len,reverse=True):text=re.sub(r'\b'+re.escape(symbol)+r'\b',name+'_'+symbol,text)
        definitions.append(text)
    expressions=[]
    for x in shown:
        rx,ry,rz=x['rotation_xyz_deg'];px,py,pz=x['position_mm']
        expressions.append(f"({x['part']}_main |> rotx {rx:.9g} |> roty {ry:.9g} |> rotz {rz:.9g} |> move {px:.9g} {py:.9g} {pz:.9g})")
    def balanced(xs):
        if len(xs)==1:return xs[0]
        mid=len(xs)//2
        return '('+balanced(xs[:mid])+' ⊕ '+balanced(xs[mid:])+')'
    (HERE/'tank-cutaway.coscad').write_text('// Generated review cutaway: front/right panels and small hardware omitted.\n'+'\n'.join(definitions)+'\nmain = '+balanced(expressions)+'\n')
    H=[r['water_mm'] for r in rows]
    fig,axs=plt.subplots(1,3,figsize=(15,4.4))
    for key,label,color in [('wall_linear_mm','Worst wall panel','#b45309'),('floor_linear_mm','Worst floor panel','#e76f51'),('frame_mm','Rigid-jointed frame','#2563eb')]:
        axs[0].plot(H,[r[key] for r in rows],'o--' if 'linear' in key else 'o-',label=label,color=color)
    axs[0].axhline(1,color='#64748b',linestyle=':',label='Panel small-deflection screen (t/2)')
    axs[0].set(ylabel='Displacement (mm)',title='Panel numbers exceed linear range');axs[0].legend(fontsize=8)
    axs[1].plot(H,[r['frame_axial_bending_mpa'] for r in rows],'o-',color='#2563eb',label='Frame axial + bending envelope')
    axs[1].plot(H,[r['bracket_unit_fixture_peak_mpa'] for r in rows],'s-',color='#dd6b20',label='Seam bracket, ideal fixture')
    axs[1].set(ylabel='Stress (MPa)',title='Separate component estimates');axs[1].legend(fontsize=8)
    axs[2].plot(H,[r['wall_force_n']/1000 for r in rows],'o-',color='#0f766e',label='Force on one wall')
    axs[2].plot(H,[r['litres']*9.80665/1000 for r in rows],'o-',color='#334155',label='Water weight')
    axs[2].set(ylabel='Force (kN)',title='Hydrostatic loads');axs[2].legend(fontsize=8)
    for ax in axs:ax.set_xlabel('Water depth (mm)');ax.grid(alpha=.2);ax.set_xticks(H)
    fig.suptitle('Four-height component screening • no seal, contact, creep or large-deflection qualification',fontsize=12)
    fig.tight_layout();fig.savefig(HERE/'four-heights.png',dpi=170,bbox_inches='tight');plt.close(fig)
    fig,axs=plt.subplots(2,2,figsize=(10,9))
    for i,d in enumerate(full['panel_detail']):
        f=d['field'];xy=np.array(f['xy_mm']);tri=mtri.Triangulation(xy[:,0],xy[:,1],np.array(f['triangles']))
        for j,(vals,title) in enumerate([(np.array(f['w_mm']),'Linear bowing (mm)'),(np.array(f['element_vm_mpa']),'Linear surface stress (MPa)')]):
            if j:plot=axs[i,j].tripcolor(tri,facecolors=vals,cmap='magma',vmax=np.quantile(vals,.98))
            else:plot=axs[i,j].tripcolor(tri,vals,cmap='viridis',shading='gouraud')
            fig.colorbar(plot,ax=axs[i,j]);axs[i,j].set_aspect('equal');axs[i,j].set_title(d['face'].capitalize()+': '+title)
            axs[i,j].set(xlabel='Local X (mm)',ylabel='Local Y (mm)')
    fig.suptitle('Full-height linear diagnostics — NOT reliable deformed shapes / failure stresses\nHoles omitted from plate model; stress colour clipped at 98th percentile to show the field',fontsize=11)
    fig.tight_layout();fig.savefig(HERE/'panel-fields.png',dpi=160,bbox_inches='tight');plt.close(fig)
    # Frame deformation is a different model: no panel membrane stiffness.
    fig=plt.figure(figsize=(8,8));ax=fig.add_subplot(111,projection='3d');frame=full['frame'];coords=np.array(frame['nodes_mm']);disp=np.array(frame['displacements_mm']);lookup={tuple(np.round(p,7)):i for i,p in enumerate(coords)}
    for m in frame['members']:
        ids=[lookup[tuple(np.round(m[k],7))] for k in ('a','b')];a=coords[ids];bpt=a+30*disp[ids]
        ax.plot(*a.T,color='#cbd5e1',linewidth=.6);ax.plot(*bpt.T,color='#2563eb',linewidth=1)
    ax.set_box_aspect([1,1,1]);ax.view_init(25,-55);ax.set(xlabel='X (mm)',ylabel='Y (mm)',zlabel='Z (mm)');ax.set_title('Ideal rigid-jointed frame at 1000 mm water\nBlue displacement exaggerated 30×; grey undeformed')
    fig.savefig(HERE/'frame-deformation.png',dpi=160,bbox_inches='tight');plt.close(fig)
    cells=''.join('<tr>'+''.join(f'<td>{r[k]:.2f}</td>' for k in ['water_mm','litres','wall_linear_mm','floor_linear_mm','frame_mm','frame_axial_bending_mpa','max_tab_n','bracket_unit_fixture_mm'])+'</tr>' for r in rows)
    stressrows=''.join('<tr>'+''.join(f'<td>{r[k]:.2f}</td>' for k in ['water_mm','wall_linear_peak_mpa','floor_linear_peak_mpa','wall_area_p95_mpa','floor_area_p95_mpa','bracket_unit_fixture_peak_mpa','m5_single_shear_nominal_mpa'])+'</tr>' for r in rows)
    page=f'''<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>2020 tank — four water heights</title>
<style>body{{font:16px/1.6 system-ui;margin:0;background:#f6f8fb;color:#172337}}main{{max-width:1160px;margin:auto;padding:32px 20px}}h1{{font-size:38px;line-height:1.2}}h2{{margin-top:36px}}.badge{{color:#9a3412;background:#ffedd5;padding:6px 12px;border-radius:20px;font-size:13px}}.note{{border-left:4px solid #c2410c;padding:16px 20px;background:#fff7ed}}img{{max-width:100%;border-radius:12px}}.scroll{{overflow:auto;background:white;border-radius:10px}}table{{border-collapse:collapse;white-space:nowrap;width:100%;font-size:14px}}td,th{{padding:12px;text-align:right;border-bottom:1px solid #e2e8f0}}th{{background:#e8eef5}}a{{color:#155eaa}}.grid{{display:grid;grid-template-columns:repeat(auto-fit,minmax(280px,1fr));gap:24px}}button{{padding:10px 18px;margin:4px;border:1px solid #b4c2d4;border-radius:8px;background:white;cursor:pointer}}#detail{{padding:20px;background:#e0edf8;border-radius:10px}}footer{{color:#64748b;font-size:13px;margin-top:40px}}</style>
<main><span class="badge">Experimental component FEA • 26 October 2026</span><h1>A 2020-framed tank with thinner PLA panels</h1><p>200 mm grid, 125 panels, 2.0 mm PLA. Approximately 1.02 m frame width/depth and 1.06 m post height; 976 × 976 × 1000 mm water space. Open top, sealed rigid-panel concept.</p>
<div class="note"><strong>The requested thin-panel design is not established as a usable water tank.</strong> With rail bearing included, every tested depth exceeds the panel small-deflection screen. The panel values below are linear diagnostics, not trustworthy final deflections or failure stresses. Nonlinear panel/contact analysis and a defined sealing/joint system are needed before a fill recommendation.</div>
<img src="tank-cutaway.png" alt="Tank cutaway with extrusion grid and panel tiles"><h2>Four water depths</h2><p>The water load uses ρgh, integrated over the wetted height; wall force grows with the square of the depth. Bottom pressure at 1 m is 9.81 kPa; one wall carries 4.79 kN and the water weighs 9.34 kN.</p>
<div>{''.join(f'<button onclick="show({i})">{r["water_mm"]} mm</button>' for i,r in enumerate(rows))}</div><div id="detail"></div>
<div class="scroll"><table><tr><th>Depth mm</th><th>Water L</th><th>Wall linear mm*</th><th>Floor linear mm*</th><th>Frame mm†</th><th>Frame stress MPa†</th><th>Max tab N</th><th>Bracket mm‡</th></tr>{cells}</table></div>
<p>* Panel bending relative to assumed fixed supports; do not add these to the separate frame result as a coupled prediction. † Rigid joints, assumed torsional stiffness, original base-grid nodes fixed. Axial + bending stress envelope, not yield qualification. ‡ Loaded-face average in the ideal clamped-back seam-bracket fixture; connector compliance is not fed into the frame model.</p><img src="four-heights.png" alt="Displacement, component stress and hydrostatic loads across four heights">
<h2>Stress diagnostics</h2><div class="scroll"><table><tr><th>Depth mm</th><th>Wall peak MPa*</th><th>Floor peak MPa*</th><th>Wall area P95*</th><th>Floor area P95*</th><th>Bracket peak MPa‡</th><th>M5 nominal shear MPa§</th></tr>{stressrows}</table></div>
<p>* Linear plate bending stress; peaks near restraint transitions are mesh-dependent, and large-deflection effects are missing. P95 is area-weighted and is also not a strength criterion. § Gross 5 mm shank, single-shear calculation using the tab force only. This is not fastener FEA. M3 pull-through, M5 prying/preload, T-nut pull-out and frame-joint bolt demands have not been qualified.</p>
<div class="grid"><img src="panel-fields.png" alt="Linear panel displacement and stress fields"><img src="frame-deformation.png" alt="Exaggerated ideal frame deformation"></div>
<h2>What was modeled</h2><ul><li>Native CoScad geometry reuses panel holes, seam brackets, T-nut previews, slotted 2020 rail geometry, flat connectors and inside-corner connectors. Boundary panels are trimmed/extended to close the approximately 1 m enclosure; they are not all identical stock squares.</li><li>Panel analysis: thin-plate Morley FEA, 2 mm isotropic PLA, E=2300 MPa, ν=0.35. Four finite tabs plus bilateral normal-restraint lines at rail inner faces. These lines idealize contact and can carry tension; separation must be checked in a contact model. Holes/fillets and nonlinear membrane/contact behaviour are omitted. Separate bracket-only and ideal edge cases are retained in the JSON.</li><li>Frame analysis: 3D beam FEA, E=69000 MPa, section A={summary['section']['area_mm2']:.1f} mm² and I={summary['section']['Iy_mm4']:.1f} mm⁴ from the modeled slotted rail, not a solid square. Actual purchased profile/alloy and torsion constant are unknown. Full-height torsion sensitivity gave about 3.06–3.32 mm.</li><li>Existing PETG seam bracket: actual native STL, Gmsh tetrahedra, quadratic solid FEA, E=2000 MPa. One-newton flange load scaled to each case. The fully clamped back face is optimistic compared with an actual bolted/friction joint.</li><li>Seals at every tile seam, corner and screw hole are required but unspecified. Rounded edges leave gaps to bridge; no seal stiffness or leak resistance is credited. Foundation, anchorage, long-term PLA creep, print anisotropy and water aging are not analyzed.</li></ul>
<h2>Checks and reproducibility</h2><p>Hydrostatic force/moment and solver residual checks pass. The simply supported plate agrees with the independent Navier series to within 0.3%; beam cantilever and point-load force/moment checks pass. Named panel displacement refinements pass 5%, as does the frame mesh; bracket displacement changes {delta*100:.2f}% between 2 and 1.5 mm meshes. Numerical convergence does not cure the missing large-deflection physics.</p><p>The full assembly parses. Focused native checks of a panel bay, frame corner and floor/wall corner have zero overlaps. The full 3,729-instance assembly, schematic threads and purchased hardware fits have not undergone a complete contact/interference qualification.</p>
<p><a href="README.md">Run instructions and assumptions</a> · <a href="results.csv">Results CSV</a> · <a href="tank.assemble">Full CoScad assembly</a> · <a href="tank-cutaway.coscad">CoScad cutaway</a> · <a href="evidence/summary.json">Numerical evidence</a></p>
<h2>Sources</h2><p><a href="https://www.grc.nasa.gov/www/k-12/WindTunnel/Activities/fluid_pressure.html">NASA hydrostatic pressure</a>; <a href="https://prusament.com/wp-content/uploads/2022/10/PLA_Prusament_TDS_2021_10_EN.pdf">Prusament PLA reference modulus</a> (not a calibration of this spool/print); <a href="https://raw.githubusercontent.com/kinnala/scikit-fem/12.0.2/docs/examples/ex02.py">scikit-fem Kirchhoff plate formulation</a>.</p>
<footer>Prepared as a design experiment. No tank was printed, assembled, filled or dispatched to a printer.</footer></main>
<script>const data={json.dumps(rows)};function show(i){{const r=data[i];document.getElementById('detail').textContent=`${{r.water_mm}} mm water: ${{r.litres.toFixed(1)}} L · ${{r.bottom_kpa.toFixed(2)}} kPa at the bottom · ${{(r.wall_force_n/1000).toFixed(2)}} kN per wall. Frame estimate ${{r.frame_mm.toFixed(2)}} mm. Panel small-deflection validity: exceeded.`;}}show(3);</script></html>'''
    (HERE/'report.html').write_text(page)
    print('report, CSV, figures and CoScad cutaway written')
if __name__=='__main__':main()
