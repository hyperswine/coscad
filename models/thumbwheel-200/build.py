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
R=15;mod=1.5;n=20;alpha=math.radians(20);rb=R*math.cos(alpha);gear=[]
for i in range(n):
 radii=[R-1.25*mod]+[rb+(R+mod-rb)*j/6 for j in range(7)]
 for side,rs in [(-1,radii),(1,reversed(radii))]:
  for r in rs:
   a=math.acos(min(1,rb/r)); ang=(i+.5)*2*math.pi/n+side*(math.pi/(2*n)+math.tan(alpha)-alpha-math.tan(a)+a)
   gear.append((r*math.cos(ang),r*math.sin(ang)))
# Fixed guide posts bear against the square ends of a closed rack slot.
travel=200
post_offset=10
post_radius=2
slot_length=travel+2*(post_offset+post_radius)
base=union([box(242,23,3,0,21,0),disk(21,3,0,0,0),disk(3,13,0,0,3),
            *[disk(post_radius,8.4,x,24,3) for x in [-post_offset,post_offset]]])
write('base',base)
pitch=math.pi*mod
rack=[(-120,30),(-120,R+1.25*mod)]
for i in range(-25,26):
 for y,side in [(R+1.25*mod,-1),(R-.95*mod,-1),(R-.95*mod,1),(R+1.25*mod,1)]:
  rack.append((i*pitch+side*(pitch/4-.11+(y-R)*math.tan(alpha)),y))
rack += [(120,R+1.25*mod),(120,30)]
car=poly(rack,5)+' ⊖ '+box(slot_length,4.55,8,0,24,-1)
write('rack',car)
# Integral scalloped wheel lies fully inside the pinion root diameter.
rim=[((12+.65*math.cos(16*a))*math.cos(a),(12+.65*math.cos(16*a))*math.sin(a))
     for a in [i*2*math.pi/256 for i in range(256)]]
wheel=union([poly(gear,5),move(poly(rim,5),0,0,5)])+' ⊖ '+disk(3.27,12,0,0,-1)
# Keeper bottoms on post tips in blind sockets, setting running clearance.
# Print both retainers upside down (closed face on bed).
keeper=box(27,8,4,0,0,0)
for x in [-10,10]:keeper+=' ⊖ '+disk(2.01,3.2,x,0,1)
cap=disk(5.3,4,0,0,0)+' ⊖ '+disk(3.01,2.9,0,0,1.3)
write('controls',union([wheel,move(keeper,36,0,0),move(cap,0,24,0)]))
assert slot_length/2-post_radius-post_offset==100
assert 25*pitch-100>3*pitch
