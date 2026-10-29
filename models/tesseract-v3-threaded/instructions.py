#!/usr/bin/env python3
"""Write the build-instruction variant of Tesseract III's assembly.

  python3 instructions.py tesseract.assemble OUT.assemble

The output has the same parts, frames, mates and poses as tesseract.assemble,
plus one `joint` line for each joint the build makes, so `coscad plan` knows
how each part goes on: every strut threads into the inner frame, every hub
onto its strut and every cap into its hub (3 mm pitch), and every bar drops
into its two open hubs. The axes come from the mates. Nothing else is added:
the planner works out the order (hubs before the bars they hold, bars before
the caps that capture them) from the meshes. tesseract.assemble itself is
left alone because verify.py and the print records hash it.
scripts/build-manual.sh uses this for the manual's Tesseract build.
"""
import re, sys
from pathlib import Path
src, out = Path(sys.argv[1]), Path(sys.argv[2])
text = src.read_text()
pitch = re.search(r'^pitch = (\S+)', (src.parent / 'hub.coscad').read_text(), re.M).group(1)
lines = ['', '// Build instructions only: how each joint is made.']
for child, parent in re.findall(r'^mate (\w+) @\w+ (\w+) @\w+', text, re.M):
    if child.startswith('bar'):
        lines.append(f'joint drop {child} {parent}')
    else:  # strut into inner, hub onto strut, cap into hub: all threaded
        lines.append(f'joint screw {child} {parent} pitch={pitch}')
out.write_text('// Generated from tesseract.assemble by instructions.py; edit that file.\n' + text.rstrip('\n') + '\n' + '\n'.join(lines) + '\n')
