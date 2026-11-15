"""Build closed-shell wings and compare spanwise bending by beam elements."""
import bisect
import re
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
    xs = [a+(b-a)*i/3 for i in range(4)]
    low = lambda q: max(0.,p[0][1]-(q-x))+.2
    profile = [(a,low(a)), (b,low(b))]+[(q,max(low(q)+.02,height(q)-.26)) for q in reversed(xs)]
    return f'{s:g} (poly ' + ' '.join(f'{q:.5f} {z:.5f}' for q,z in profile)+')'

cuts = []
for a,b in zip(ribs,ribs[1:]):
    if not openings((a+b)/2):
        continue
    for lo,hi in bays:
        cuts.append('(θ 90 (loft '+cutter(a+0.45,lo,hi)+' '+cutter(b-0.45,lo,hi)+'))')
frame = 'outline = '+source+'\nframe = outline ⊖ ('+' ⊕ '.join(cuts)+')\n'
for name,shape in [('right','frame'),('left','⇋ 0 1 0 frame')]:
    (root/f'wing-{name}.coscad').write_text(frame+'main = '+shape+'\n')

def properties(s, candidate):
    p = section(s)
    x0 = p[0][0]
    c = p[2][0]-x0
    upper = sorted([p[0]]+p[3:])
    xs = [x for x,z in upper]
    area = first = second = 0.
    for j in range(160):
        u = (j+.5)/160
        x = x0+c*u
        i = min(len(xs)-2,bisect.bisect_right(xs,x)-1)
        a,z = upper[i]
        b,w = upper[i+1]
        h = z+(w-z)*(x-a)/(b-a)
        bot = max(0.,p[0][1]-(x-x0))
        if candidate and any(l<u<r for l,r in openings(s)):
            core = 0
        # Nominal slicer shells and homogenized 8% core. Thin rails bounded
        # by two perimeters on both sides become solid locally.
        edges = [0,1]+([v for pair in openings(s) for v in pair] if candidate else [])
        core = 0 if candidate and any(l<u<r for l,r in openings(s)) else 1 if min(abs(u-e)*c for e in edges)<(.42 if candidate else .84) else (.05 if candidate else .08)
        lower = min(h,bot+(.2 if candidate else .4))
        top = max(lower,h-(.26 if candidate and core==0 else (.4 if candidate else .6)))
        for l,r,d in [(bot,lower,1),(lower,top,core),(top,h,1)]:
            width = c/160*d
            area += width*(r-l)
            first += width*(r*r-l*l)/2
            second += width*(r**3-l**3)/3
    neutral = first/area
    return second-area*neutral**2,max(neutral,max(xs_z[1] for xs_z in upper)-neutral)

def solve(candidate, step):
    # Euler-Bernoulli cantilever elements. Recover displacement by
    # solving each element successively from its known left nodal motion.
    L = 236
    clamp = 20
    n = round((L-clamp)/step)
    dz = (L-clamp)/n
    load = 1.0/(L-clamp)
    delta = slope = stress = 0.
    for j in range(n):
        s = clamp+(j+.5)*dz
        inertia,extreme = properties(s,candidate)
        moment = load*(L-s)**2/2
        # Hermite beam element, consistent uniform load. End forces follow
        # directly from equilibrium because the cantilever is determinate.
        free = L-s-dz/2
        shear = load*(free+dz/2)
        end_moment = load*(free**2/2-dz**2/12)
        ei = 2500*inertia
        delta += slope*dz+(shear*dz**3/3+end_moment*dz**2/2)/ei
        slope += (shear*dz**2/2+end_moment*dz)/ei
        stress = max(stress,moment*extreme/inertia)
    return dict(tip_mm=delta,peak_nominal_MPa=stress)
if __name__ == '__main__':
    for step in [1,.25,.125]:
        print(step,solve(False,step),solve(True,step))
