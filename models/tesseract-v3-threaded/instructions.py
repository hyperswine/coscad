#!/usr/bin/env python3
"""Write the build-instruction variant of Tesseract III's assembly.

  python3 instructions.py tesseract.assemble OUT.assemble

The output has the same parts, frames, mates and poses as tesseract.assemble,
plus what only the build instructions need: every integral printed joint
(strut threads into the inner frame, hub onto strut, cap into hub, each bar
end into its two hubs) as a `fastener peg` line so `coscad plan` knows what
holds what, and one ordering rule: each cap goes on after the bars it
captures. tesseract.assemble itself is left alone because verify.py and the
print records hash it. The planner has no joint type for an integral thread,
so the steps say "push ... until it seats" for the threaded joints too: screw
those home. scripts/build-manual.sh uses this for the manual's Tesseract build.
"""
import re, sys
from pathlib import Path
src, out = Path(sys.argv[1]), Path(sys.argv[2])
text = src.read_text()
# corner hubs: i -> signs of the hub centre (+-60, +-60, +-60 mm in the solved assembly)
hubs = {1:(1,1,1),2:(-1,1,1),3:(-1,-1,1),4:(1,-1,1),5:(1,1,-1),6:(-1,1,-1),7:(-1,-1,-1),8:(1,-1,-1)}
# bars: name -> (axis, hub at the low end, hub at the high end), from the mates
bars = {}
for bar, frame, hub in re.findall(r'^mate (bar\d+) @(left|right) (hub\d+) @edge_[xyz]', text, re.M):
    bars.setdefault(bar, []).append(int(hub[3:]))
axis_of = lambda a, b: next(i for i in range(3) if hubs[a][i] != hubs[b][i])
neg, pos = ['lft', 'fwd', 'bot'], ['rt', 'bak', 'top']
lines = ['', '// Build instructions only: integral printed joints (threads and drop-in',
         '// sockets) recorded as peg joints so `coscad plan` knows what holds what.']
for i, (sx, sy, sz) in hubs.items():
    side = 'rt' if sx > 0 else 'lft'
    lines += [f'fastener peg strut{i} inner {side}', f'fastener peg hub{i} strut{i} {side}',
              # the cap's retaining face sits 1.8 mm inside the hub's outer face
              f'fastener peg cap{i} hub{i} {"top" if sz > 0 else "bot"} inset=1.8']
captured = {i: [] for i in hubs}
for bar, ends in sorted(bars.items(), key=lambda kv: int(kv[0][3:])):
    a = axis_of(*ends)
    for h in ends:
        face = neg[a] if hubs[h][a] > 0 else pos[a]   # the hub face that looks at the bar
        # each bar end seats 10.4 mm inside its hub (hub face at 40 mm, bar end at 50.4)
        lines.append(f'fastener peg {bar} hub{h} {face} inset=10.4')
        captured[h].append(bar)
def after(m):
    i = int(m.group(1))
    return m.group(0) + ' after=' + ','.join(captured[i])
text = re.sub(r'^cap(\d+) ← cap\.coscad[^\n]*', after, text, flags=re.M)
out.write_text('// Generated from tesseract.assemble by instructions.py; edit that file.\n' + text.rstrip('\n') + '\n' + '\n'.join(lines) + '\n')
