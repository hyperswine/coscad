"""Generate the short-travel hand-operated demonstrator, dimensions in mm."""
import math
from pathlib import Path
root=Path(__file__).resolve().parent
def profile(points,height):
    return f'(⮕ {height} (poly '+' '.join(f'{x:.4f} {y:.4f}' for x,y in points)+'))'
def circle(r,h,x,y,z):
    return f'(zcyl {r} {h} |> move {x} {y} {z+h/2})'
def box(w,d,h,x,y,z):
    return f'(box {w} {d} {h} |> move {x} {y} {z+h/2})'
def save(name,expr):
    (root/(name+'.coscad')).write_text('main = '+expr+'\n')
# Archimedean strip joined to the hub and an off-axis anchor eye.
turn=2*math.pi*1.5
spiral=[]
for side,indices in [(1,range(181)),(-1,reversed(range(181)))]:
    for i in indices:
        t=turn*i/180
        r=3.0+7*t/turn+side*.35
        spiral.append((r*math.cos(t),r*math.sin(t)))
spring=profile(spiral,1.2)
eye=circle(2.4,1.2,-11,0,0)
# Module 1, 16 teeth. Approximate involute flanks with six segments.
R=8; alpha=math.radians(20); rb=R*math.cos(alpha)
def halfangle(r):
    a=math.acos(min(1,rb/r))
    return math.pi/32+(math.tan(alpha)-alpha)-(math.tan(a)-a)
gear=[]
for i in range(16):
    t=i*math.pi/8
    radii=[6.75]+[rb+(9-rb)*j/6 for j in range(7)]
    for side,rs in [(-1,radii),(1,reversed(radii))]:
        for r in rs:
            a=t+side*halfangle(r)
            gear.append((r*math.cos(a),r*math.sin(a)))
# A D-shaped drive aligns a flat-printed pinion on the spring hub.
def drive(radius,flat,height):
    return '('+circle(radius,height,0,0,0)+' ∩ '+box(12,12,height,flat-6,0,0)+')'
hub=circle(3.5,2,0,0,0)
key='('+drive(3.3,2.45,2)+' |> move 0 0 2)'
save('rotor','('+spring+' ⊕ '+eye+' ⊕ '+hub+' ⊕ '+key+') ⊖ '+circle(1.85,6,0,0,-1)+' ⊖ '+circle(1.35,3,-11,0,-1))
save('pinion',profile(gear,2)+' ⊖ '+drive(3.48,2.63,2))
# Carriage print frame is also its operating orientation. Two guide posts
# in closed slots limit the usable stroke to approximately 6 mm.
pitch=math.pi
rack=[(-15,16),(-15,9.25)]
for i in range(-4,5):
    x=(i+.5)*pitch
    for y,dx in [(9.25,-1),(7.05,-1),(7.05,1),(9.25,1)]:
        half=pitch/4-.10+(y-8)*math.tan(alpha)
        rack.append((x+dx*half,y))
rack += [(16,9.25),(16,16)]
def slot(x):
    return '('+circle(1.55,4,x-3,13,-1)+' ⇓ '+circle(1.55,4,x+3,13,-1)+')'
save('carriage',profile(rack,2)+' ⊖ '+slot(-8)+' ⊖ '+slot(8))
base=box(38,33,2,0,2,-2)
platform=box(38,7,2,0,14,0)
base+=' ⊕ '+platform+' ⊕ '+circle(1.55,4.4,0,0,0)+' ⊕ '+circle(1.05,1.4,-11,0,0)
for x in (-8,8):
    base+=' ⊕ '+circle(1.25,5.6,x,13,0)
save('base','('+base+') |> move 0 0 2')
# Removable friction-fit keeper spans both guide posts, above carriage.
keeper=box(21,4.5,1.4,0,13,0)
for x in (-8,8):
    keeper+=' ⊖ '+circle(1.23,3,x,13,-1)
save('keeper',keeper)
# All five pieces have flat build faces.
parts=[('base',0,0),('rotor',36,0),('carriage',36,8),('keeper',0,15),('pinion',64,0)]
expr=[]
for name,x,y in parts:
    shape=(root/(name+'.coscad')).read_text().split('main = ')[1].strip()
    expr.append(f'(({shape}) |> move {x} {y} 0)')
save('plate',' ⊕ '.join(expr))
