"""Generate a CoScad captive globe, using deliberately simplified coastlines.
No external map dataset; coordinates are hand-drawn longitude/latitude outlines.
Run with the mesh environment documented in README.md.
"""
from pathlib import Path
import math, json, subprocess
from shapely.geometry import Polygon
from shapely.ops import triangulate
BASE=Path(__file__).resolve().parent
ROOT=BASE.parents[1]
LAND={
 'north_america':[(-168,70),(-145,69),(-126,73),(-100,74),(-80,66),(-58,53),(-65,45),(-77,35),(-81,25),(-87,20),(-83,10),(-94,16),(-105,23),(-117,31),(-125,49),(-140,59),(-164,60)],
 'south_america':[(-81,11),(-71,11),(-61,8),(-51,3),(-35,-5),(-39,-20),(-50,-30),(-56,-52),(-66,-55),(-73,-40),(-78,-15),(-82,-3)],
 'eurasia':[(-10,36),(-9,44),(-2,49),(-6,56),(9,58),(6,71),(27,71),(38,64),(60,72),(100,77),(140,72),(178,64),(168,54),(145,45),(140,35),(130,30),(121,23),(109,19),(109,6),(103,1),(99,10),(90,22),(80,7),(73,15),(67,25),(56,25),(52,15),(43,12),(35,30),(27,36),(22,38),(15,36),(8,43),(0,40)],
 'africa':[(-17,29),(-6,36),(12,37),(25,31),(33,31),(43,12),(51,11),(44,1),(40,-12),(32,-26),(20,-35),(12,-25),(10,-6),(1,4),(-10,5),(-17,15)],
 'greenland':[(-53,59),(-43,60),(-23,70),(-20,81),(-43,83),(-61,76)],
 'australia':[(113,-22),(122,-15),(130,-12),(137,-17),(142,-11),(151,-23),(154,-29),(146,-39),(134,-34),(122,-35),(114,-29)],
 'madagascar':[(44,-13),(50,-15),(47,-26),(43,-23)],
 'japan':[(130,31),(136,34),(141,41),(144,44),(142,36),(136,32)],
 'britain':[(-6,50),(-3,59),(0,57),(1,52)],
 'new_zealand':[(166,-46),(171,-40),(178,-36),(178,-40),(171,-47)],
 'indonesia':[(96,5),(105,-3),(117,-5),(128,-3),(138,-5),(139,-8),(127,-9),(115,-8),(104,-6)]
}

def move(x,y,z,shape):return f'χ {x:.6f} (ψ {y:.6f} (ζ {z:.6f} ({shape})))'
def ray_point(lon,lat):
 lon,lat=map(math.radians,(lon-90,lat))
 return (100*math.cos(lat)*math.cos(lon),100*math.cos(lat)*math.sin(lon),100*math.sin(lat))
lines=['// Millimetres. Captive vertical axle; assembled orientation is the print orientation.',
 '// 64 mm sphere; 0.8 mm relief; 86 mm base; 94 mm total height.',
 'radius = 32', 'center_z = 54','pin_r = 2.5','bore_r = 3.1',
 'base_lower = ◎ 43 5', 'base_upper = ζ 6.8 (◎ 41 0.2)',
 'base = base_lower ⇓ base_upper', 'pedestal = ζ 6 (◎ 9 12)',
 'meridian = ζ center_z (θ 90 (ζ -3 ((◎ 39.5 6) ⊖ (ζ -0.1 (◎ 36 6.2)))))',
 'shaft = ζ 16 (◎ pin_r 78)',
 'lower_bearing = ζ 18 (◎ 4.5 6.4)',
 '// Top collar: short 45 degree expansion above the globe, no bearing overhang cavity.',
 'upper_bearing = (ζ 86.6 (◎ 2.5 0.1)) ⇓ (ζ 88.6 (◎ 4.5 3.5))',
 'stand = base ⊕ pedestal ⊕ meridian ⊕ shaft ⊕ lower_bearing ⊕ upper_bearing',
 'ocean = ● radius','relief_shell = (● 32.8) ⊖ (● 31.9)',
 '// Tiny boxes define each radial triangle wedge; all wedges overlap the solid ocean.',
 'point = χ -0.005 (ψ -0.005 (ζ -0.005 (▬ 0.01 0.01 0.01)))']
wedges=[]
for name,coords in LAND.items():
 poly=Polygon(coords)
 assert poly.is_valid,name
 for tri in triangulate(poly):
  if not poly.covers(tri.representative_point()):continue
  # All Delaunay triangles retained must be entirely inside the outline.
  assert poly.covers(tri),name
  w=f'{name}_{len(wedges)}';wedges.append(w)
  points=[move(*ray_point(*xy),'point') for xy in list(tri.exterior.coords)[:3]]
  lines.append(w+' = point ⇓ '+' ⇓ '.join(points))
lines += ['continent_wedges = '+' ⊕ '.join(wedges),
 'antarctic_cap = ζ -40 (χ -40 (ψ -40 (▬ 80 80 10)))',
 '// Polar trim keeps relief off both thrust-bearing surfaces.',
 'polar_trim = ζ -40 (◎ 6 80)',
 'land = ((continent_wedges ⊕ antarctic_cap) ∩ relief_shell) ⊖ polar_trim',
 'bore = ζ -40 (◎ bore_r 80)',
 'mouth = (ζ -30.8 (◎ 6.5 0.01)) ⇓ (ζ -27.4 (◎ 3.1 0.01))',
 'lower_cut = χ -40 (ψ -40 (ζ -40 (▬ 80 80 9.2)))',
 'rotor = ζ center_z ((ocean ⊕ land) ⊖ bore ⊖ mouth ⊖ lower_cut)',
 '// Support exclusion extends across both bearing clearances and the complete bore.',
 'bearing_blocker = ζ 20.5 (◎ 5.5 69)',
 'main = stand ⊕ rotor']
source='\n'.join(lines)+'\n'
(BASE/'globe.coscad').write_text(source)
compiler=ROOT/'dist-newstyle/build/aarch64-osx/ghc-9.8.2/coscad-1.2.0.0/x/coscad/build/coscad/coscad'
for name,main in [('globe','stand ⊕ rotor'),('stand','stand'),('rotor','rotor'),('bearing-blocker','bearing_blocker'),('land','ζ center_z land'),('ocean','ζ center_z (ocean ⊖ bore)')]:
 src=BASE/(name+'.coscad')
 if name!='globe':src.write_text(source.rsplit('main =',1)[0]+'main = '+main+'\n')
 out=BASE/(name+'.scad')
 subprocess.run([str(compiler),str(src),'-o',str(out)],check=True)
 out.write_text(out.read_text().replace('$fn = 50;','$fn = 96;'))
 if name!='globe':src.unlink()
(BASE/'design.json').write_text(json.dumps({'sphere_diameter_mm':64,'relief_mm':0.8,'base_diameter_mm':86,'height_mm':94,'axle_diameter_mm':5,'bore_diameter_mm':6.2,'radial_clearance_mm':0.6,'lower_collar_top_z':24.4,'bottom_flat_z':23.2,'flared_bore_bottom_radius_mm':6.5,'flared_bore_top_z':26.6,'lower_bearing_axial_clearance_mm':0.8,'sphere_center_z':54,'support_blocker_radius_mm':5.5,'tree_supports':'on base and bed; bearing gap excluded','land_source':'hand-drawn simplified longitude/latitude polygons; illustrative, not a geographic dataset'},indent=2)+'\n')
print('Generated',len(wedges),'continental wedges and assembled captive globe')
