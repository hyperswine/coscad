"""Compare straight extruding moves labelled as bridges in the actual plates."""
from pathlib import Path
import re, math
for printer in ['a1-wing-right','p1s-wing-left']:
 for folder in ['tube/prototype','supported']:
    gcode=Path(__file__).resolve().parent.parent/folder/printer/'slice/plate_1.gcode'
    x=y=0.;bridge=False;lengths=[]
    for line in gcode.read_text().splitlines():
        if line.startswith('; FEATURE:'): bridge='bridge' in line.lower()
        if line.startswith(('G0 ','G1 ')):
            f=dict((k,float(v)) for k,v in re.findall(r'([XYE])(-?[\d.]+)',line.split(';')[0]))
            a,b=f.get('X',x),f.get('Y',y)
            if bridge and f.get('E',0)>0 and (a,b)!=(x,y):lengths.append(math.hypot(a-x,b-y))
            x,y=a,b
        if 'total filament weight [g]' in line:mass=line.split(':')[-1].strip()
    print(printer,folder,'max bridge move',round(max(lengths,default=0),2),'mm; moves >10 mm',sum(v>10 for v in lengths),'plate grams',mass)
