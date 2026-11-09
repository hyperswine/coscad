#!/usr/bin/env python3
"""Repack the whole cube kit into two printer-specific beds; slice without printer access."""
import argparse
from collections import Counter
import hashlib
import importlib.util
import json
from pathlib import Path
import subprocess
import zipfile
import trimesh

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
TANK = ROOT / 'models/tank-model-2020'
JOBS = {
    'p1s': dict(job='p1s-panels-brackets-20261109', machine='Bambu Lab P1S 0.4 nozzle',
                model='Bambu Lab P1S', model_id='C12', filament='Bambu PLA Basic @BBL P1S 0.4 nozzle',
                process='0.20mm Standard @BBL X1C', layer_height=.2, walls='4', infill='100%', spool='external',
                layout=[('panel-print', (128, 60)), ('panel-print', (128, 112))]
                       + [('bracket', (x,y)) for y in (164,210) for x in (48,100,152,204)]),
    'a1': dict(job='a1-rails-brackets-20261109', machine='Bambu Lab A1 0.4 nozzle',
               model='Bambu Lab A1', model_id='N2S', filament='Bambu PLA Basic @BBL A1',
               process='0.20mm Standard @BBL A1', layer_height=.2, walls='4', infill='25%', spool='ams0',
               layout=[('rail-print-150' if y==60 else 'rail-print-110',(x,y)) for y in (60,98,136) for x in (52,88,124,160)]
                      + [('bracket',(212,y)) for y in (54,100,146,192)]
                      + [('bracket',(x,216)) for x in (52,104,156)] + [('bracket',(104,172.5))])}
FOLDER = HERE / JOBS['p1s']['job']  # default required by the shared audit's import


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


def prepare(device):
    job = JOBS[device]; folder=HERE/job['job']
    if (folder/'print-run.json').exists(): raise RuntimeError('Existing dispatch prevents rebuilding this job')
    validation=json.loads((HERE/'validation.json').read_text())
    if not validation['passed']: raise ValueError('Cube geometry not validated')
    for part in {p for p,_ in job['layout']}:
        for ext,key in (('.coscad','source_sha256'),('.stl','stl_sha256')):
            if digest(HERE/'parts'/(part+ext)) != validation['print_parts'][part][key]:
                raise ValueError('Part changed after geometry validation')
    module=load('cube_project_builder', TANK/'prepare_slice.py')
    module.B=HERE
    module.PROCESS=job['process']
    module.PRINTER[device]=(job['machine'],job['filament'],job['model'])
    overrides={**module.COMMON,'wall_loops':job['walls'],'sparse_infill_density':job['infill'],
               'sparse_infill_pattern':'gyroid' if device=='a1' else 'zig-zag',
               'top_shell_layers':'5','bottom_shell_layers':'5','brim_type':'no_brim','brim_width':'0',
               'print_sequence':'by layer','outer_wall_speed':['40'],'inner_wall_speed':['70'],
               'initial_layer_speed':['25'],'default_acceleration':'2000','outer_wall_acceleration':'1000',
               'travel_speed':['180'],'travel_acceleration':'3000','enable_arc_fitting':'0',
               'elefant_foot_compensation':'0.15'}
    module.JOBS={job['job']:(job['layout'],overrides)}
    def mesh(part):
        m=trimesh.load_mesh(HERE/'parts'/(part+'.stl'))
        if not m.is_watertight or len(m.split())!=1:raise ValueError('Invalid print part')
        m.apply_translation([-m.bounds[:,0].mean(),-m.bounds[:,1].mean(),-m.bounds[0,2]])
        return m
    module.mesh=mesh
    module.main([job['job']])
    for name in ('process','project','filament'):
        p=folder/'presets'/(name+'.json');s=json.loads(p.read_text())
        if name in ('process','project'):s.update(layer_height=str(job['layer_height']),initial_layer_print_height='0.2')
        if name in ('filament','project'):
            s.update(textured_plate_temp=['55'],textured_plate_temp_initial_layer=['55'],
                     nozzle_temperature=['220'],nozzle_temperature_initial_layer=['220'])
        p.write_text(json.dumps(s,indent=2)+'\n')
    parts=[]
    for index,(part,centre) in enumerate(job['layout']):
        m=mesh(part)
        parts.append(dict(name='parts/'+part,centre_mm=list(centre),object_id=2*index+2,
                          holes=(2-m.euler_number)//2,height_mm=float(m.extents[2]),
                          source_sha256=digest(HERE/'parts'/(part+'.coscad')),stl_sha256=digest(HERE/'parts'/(part+'.stl'))))
    intent=dict(printer=device,plate='Textured PEI Plate',bed_c=55,nozzle_c=220,parts=parts,
                settings=overrides,spool=job['spool'],owner_confirmed_bed_clear=True)
    (folder/'print-intent.json').write_text(json.dumps(intent,indent=2)+'\n')
    project=folder/'tank-project.3mf'
    with zipfile.ZipFile(project) as z:entries={e.filename:z.read(e) for e in z.infolist()}
    entries['Metadata/project_settings.config']=(folder/'presets/project.json').read_bytes()
    with zipfile.ZipFile(project,'w',zipfile.ZIP_DEFLATED) as z:
        for name,data in entries.items():z.writestr(name,data)
    out=folder/'slice';out.mkdir(exist_ok=True)
    cmd=[module.s.APP,'--debug','2','--load-settings',str(folder/'presets/machine.json')+';'+str(folder/'presets/process.json'),
         '--load-filaments',str(folder/'presets/filament.json'),'--arrange','0','--orient','0','--slice','1',
         '--export-3mf','cube.gcode.3mf','--outputdir',str(out),str(project)]
    (out/'slice-command.json').write_text(json.dumps(cmd,indent=2)+'\n')
    with (out/'slicer.log').open('w') as log:subprocess.run(cmd,stdout=log,stderr=log,check=True)
    print('Sliced',device,flush=True)


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('printer',choices=JOBS)
    prepare(parser.parse_args().printer)
