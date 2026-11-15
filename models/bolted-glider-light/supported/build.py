"""Build closed-shell wings and compare spanwise bending by beam elements."""
import bisect
import re
import math
from pathlib import Path

root = Path(__file__).resolve().parent
base = root.parent.parent / "bolted-glider-balanced"
source = (base / "wing-right.coscad").read_text().split("main = ")[1].strip()
stations = []
for s, points in re.findall(r'([\d.]+) \(poly ([\d. ]+)\)', source):
    values = list(map(float, points.split()))
    stations.append((float(s), list(zip(values[::2], values[1::2]))))

def section(s):
    i = min(len(stations)-2, max(0, bisect.bisect_right([v[0] for v in stations], s)-1))
    a, p = stations[i]
    b, q = stations[i+1]
    return [(x+(u-x)*(s-a)/(b-a), y+(v-y)*(s-a)/(b-a)) for (x,y),(u,v) in zip(p,q)]

# Chord fractions. Paired cavities leave two continuous spars, plus edge rails.
bays = [(0.012,0.29),(0.302,0.66),(0.672,0.976)]
ribs = [0,8,20,31,49,67,85,103,121,139,157,175,193,206,218,227,236]
def openings(s):
    if min(abs(s-r) for r in ribs)<0.45 or 8<s<20 or s>227:
        return []
    return bays

def cutter(s, lo, hi):
    p = section(s)
    x = p[0][0]
    c = p[2][0]-x
    a,b = x+c*lo,x+c*hi
    upper = sorted([p[0]]+p[3:])
    def height(q):
        i = bisect.bisect_left([x for x,z in upper],q)-1
        x,z = upper[i]
        u,w = upper[i+1]
        return z+(w-z)*(q-x)/(u-x)
    # Keep the same profile vertex correspondence through every loft.
    xs = [a,b]
    low = lambda q: max(0.,p[0][1]-(q-x))+.2
    profile = [(a,low(a)), (b,low(b))]+[(q,max(low(q)+.02,height(q)-.42)) for q in reversed(xs)]
    return f'{s:g} (poly ' + ' '.join(f'{q:.5f} {z:.5f}' for q,z in profile)+')'

cuts = []
for a,b in zip(ribs,ribs[1:]):
    if not openings((a+b)/2):
        continue
    # Partition every original bay into narrow cells. Adjacent stations
    # stagger the internal webs to make a continuous zigzag through each rib.
    for lo,hi in bays:
        ca=section(a)[2][0]-section(a)[0][0]
        cb=section(b)[2][0]-section(b)[0][0]
        count=math.ceil((hi-lo)*max(ca,cb)/7)
        def edge(j,station,chord):
            u=lo+(hi-lo)*j/count
            if 0<j<count:
                u+=(-1)**ribs.index(station)*.65/chord
            return u
        for j in range(count):
            bounds=[]
            for station,chord in [(a,ca),(b,cb)]:
                left=edge(j,station,chord)+(.25/chord if j else 0)
                right=edge(j+1,station,chord)-(.25/chord if j+1<count else 0)
                bounds.append((left,right))
            cuts.append('(θ 90 (loft '+cutter(a+.45,*bounds[0])+' '+cutter(b-.45,*bounds[1])+'))')
frame = 'outline = '+source+'\nframe = outline ⊖ ('+' ⊕ '.join(cuts)+')\n'
for name,shape in [('right','frame'),('left','⇋ 0 1 0 frame')]:
    (root/f'wing-{name}.coscad').write_text(frame+'main = '+shape+'\n')

