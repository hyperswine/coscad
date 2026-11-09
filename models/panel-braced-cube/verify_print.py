#!/usr/bin/env python3
"""Audit exact printer-specific cube geometry, inventory, paths, every layer and temperatures."""
import argparse
from collections import Counter,defaultdict
import json
import zipfile
from prepare_print import HERE,TANK,JOBS,load


def verify(device):
    job=JOBS[device];folder=HERE/job['job'];intent=json.loads((folder/'print-intent.json').read_text())
    expected=Counter('parts/'+p for p,_ in job['layout'])
    if Counter(p['name'] for p in intent['parts'])!=expected:raise ValueError('Wrong cube inventory')
    levels=defaultdict(set);height=None
    for line in (folder/'slice/plate_1.gcode').open():
        if line.startswith('; Z_HEIGHT:'):height=round(float(line.split(':')[1]),4)
        if line.startswith('; OBJECT_ID:') and height is not None:levels[int(line.split(':')[1])].add(height)
    if set(levels)!={p['object_id'] for p in intent['parts']}:raise ValueError('Missing/extra sliced objects')
    for part in intent['parts']:
        count=round((part['height_mm']-.2)/job['layer_height'])+1
        if levels[part['object_id']]!={round(.2+job['layer_height']*i,4) for i in range(count)}:
            raise ValueError('Missing layers for '+part['name'])
    path=folder/'slice/cube.gcode.3mf'
    with zipfile.ZipFile(path) as z:settings=json.loads(z.read('Metadata/project_settings.config'))
    def numbers(value):
        return [float(v) for v in (value if isinstance(value,list) else [value])]
    for key in ('default_acceleration','outer_wall_acceleration','travel_acceleration','outer_wall_speed','inner_wall_speed','travel_speed'):
        if numbers(settings[key])!=numbers(intent['settings'][key]):raise ValueError('Ignored motion setting: '+key)
    audit=load('cube_package_audit',TANK/'panel-fit-v2/verify_print.py')
    result=audit.verify(HERE,folder,expected_parts=len(intent['parts']),profile=dict(
        printer_settings_id=job['machine'],printer_model=job['model'],model_id=job['model_id'],
        package_name='cube.gcode.3mf',layer_height=job['layer_height'],wall_loops=job['walls'],sparse_infill_density=job['infill']))
    result['per_object_layers']={str(k):dict(layers=len(v),top_mm=max(v)) for k,v in levels.items()}
    (folder/'package-validation.json').write_text(json.dumps(result,indent=2)+'\n')
    return result


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('printer',choices=JOBS)
    verify(parser.parse_args().printer)
