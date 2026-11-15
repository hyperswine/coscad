"""Three-plate captive display mechanism. Dimensions in mm."""
from pathlib import Path
import math
root=Path(__file__).resolve().parent
def disk(r,h,x,y,z):return f'(zcyl {r} {h} |> move {x} {y} {z+h/2})'
def box(w,d,h,x,y,z):return f'(box {w} {d} {h} |> move {x} {y} {z+h/2})'
def poly(pts,h):return f'(⮕ {h} (poly '+' '.join(f'{x:.4f} {y:.4f}' for x,y in pts)+'))'
def union(parts):return '('+' ⊕ '.join(parts)+')'
def write(name,s): (root/(name+'.coscad')).write_text('main = '+s+'\n')
def hull(a,b):return f'({a} ⇓ {b})'
def move(s,x,y,z):return f'({s} |> move {x} {y} {z})'
# Thin, long spiral for low resistance; motion limited by closed carriage slots.
pts=[]
for side,indices in [(1,range(501)),(-1,reversed(range(501)))]:
 for i in indices:
  a=3*math.pi*i/500;r=8+48*i/500+side*.45
  pts.append((r*math.cos(a),r*math.sin(a)))
def drive(r,flat,h):return f'({disk(r,h,0,0,0)} ∩ {box(24,24,h,flat-12,0,0)})'
rotor=union([poly(pts,1.2),disk(8.6,2,0,0,0),move(drive(7,5.7,4),0,0,2),disk(4,1.2,-57,0,0)])+' ⊖ '+disk(3.8,8,0,0,-1)+' ⊖ '+disk(2.15,4,-57,0,-1)
R=36;mod=3;n=24;alpha=math.radians(20);rb=R*math.cos(alpha);gear=[]
for i in range(n):
 radii=[R-1.25*mod]+[rb+(R+mod-rb)*j/6 for j in range(7)]
 for side,rs in [(-1,radii),(1,reversed(radii))]:
  for r in rs:
   a=math.acos(min(1,rb/r)); ang=(i+.5)*2*math.pi/n+side*(math.pi/(2*n)+math.tan(alpha)-alpha-math.tan(a)+a)
   gear.append((r*math.cos(ang),r*math.sin(ang)))
pinion=union([poly(gear,4)+' ⊖ '+drive(7.2,5.9,4),disk(5.8,3,0,0,4)+' ⊖ '+disk(3.8,5,0,0,3)])
rack=[(-70,54),(-70,39.75)]
for i in range(-7,8):
 for y,side in [(39.75,-1),(33.3,-1),(33.3,1),(39.75,1)]:
  rack.append((i*math.pi*mod+side*(math.pi*mod/4-.15+(y-R)*math.tan(alpha)),y))
rack += [(70,39.75),(70,54)]
car=poly(rack,4)
for x in [-45,45]:car+=' ⊖ '+hull(disk(3.3,6,x-4.7,47,-1),disk(3.3,6,x+4.7,47,-1))
# Platform supports lie outside spring footprint; track sits above spring.
base=union([box(152,128,3,0,0,0),disk(3.45,8,0,0,3),disk(1.85,1.5,-57,0,3),*[box(30,11,2,x,49,3) for x in [-54,54]]])
posts=[(-45,47),(45,47),(0,-60)]
for x,y in posts:
 if y<0:base+=' ⊕ '+box(14,8,6.5,x,y,3)
 base+=' ⊕ '+disk(3,12,x,y,3)
 # Small crush ribs enter progressively and engage only the stationary cover.
 for dx in [-2.95,2.95]:
  base+=' ⊕ '+hull(disk(.17,.2,x+dx,y,10.3),disk(.06,.2,x+dx,y,14.6))
# Stop feet on stationary outboard pads establish axial clearance.
for x in [-69,69]:base+=' ⊕ '+box(5,5,6.5,x,58,3)
cover=union([box(144,9,5,0,57,0),box(100,10,5,0,47,0),box(12,126,5,0,-3,0),disk(11,5,0,0,0),box(10,12,5,-45,52,0),box(10,12,5,45,52,0)])
cover+=' ⊖ '+disk(6.08,7,0,0,-1)
for x,y in posts:
 # Close sliding fit over most of the hole, with entry chamfer.
 cover+=' ⊖ '+disk(3.05,7,x,y,-1)+' ⊖ '+disk(3.40,.8,x,y,-.1)
write('bed-1-base',base)
write('bed-2-drive',union([rotor,move(pinion,101,0,0)]))
write('bed-3-cover-rack',union([cover,move(car,0,43,0)]))
