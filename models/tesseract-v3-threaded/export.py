#!/usr/bin/env python3
"""Select named parts from hand-authored CoScad; does not generate geometry."""
from pathlib import Path
import re
HERE=Path(__file__).resolve().parent
PARTS={'hub-blockers':'hub_blockers','inner-blockers':'inner_blockers','bar-print':'bar_print','hub':'hub','cap':'cap_part','bar':'bar','strut':'strut','inner':'inner','coupon-socket':'coupon_socket','coupon-screw':'coupon_screw','male':'male','female-tool':'female'}
source=(HERE/'design.coscad').read_text()
assert len(re.findall(r'^main =.*$',source,re.M))==1
for name,expression in PARTS.items():
    (HERE/(name+'.coscad')).write_text('// Selected from design.coscad by export.py; edit the master.\n'+re.sub(r'^main =.*$','main = '+expression,source,flags=re.M))
print('Selected '+str(len(PARTS))+' pure CoScad definitions')
