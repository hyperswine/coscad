#!/usr/bin/env python3
"""Slice one complete upright wall using the fit samples' X1C process."""
import hashlib
import importlib.util
import json
from pathlib import Path
import subprocess
import zipfile

import trimesh

HERE = Path(__file__).resolve().parent
JOB = 'x1c-full-panel-c025-20261107'
FOLDER = HERE / JOB
PART = 'panel-wall-double-c025-upright'


def main():
    if (FOLDER / 'print-run.json').exists():
        raise RuntimeError('Existing dispatch prevents rebuilding this job')
    validation = json.loads((HERE / 'geometry-validation.json').read_text())
    if (not validation['passed']
            or validation['stl_sha256'] != hashlib.sha256((HERE / (PART + '.stl')).read_bytes()).hexdigest()
            or validation['source_sha256'] != hashlib.sha256((HERE / (PART + '.coscad')).read_bytes()).hexdigest()):
        raise RuntimeError('Geometry validation missing/stale')
    spec = importlib.util.spec_from_file_location('tank_prepare', HERE.parent / 'prepare_slice.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.B = HERE
    module.JOBS = {JOB: ([(PART, (128, 128))], {
        **module.COMMON, 'wall_loops': '4', 'sparse_infill_density': '100%',
        'top_shell_layers': '5', 'bottom_shell_layers': '5',
        'brim_type': 'no_brim', 'brim_width': '0', 'print_sequence': 'by layer',
        'outer_wall_speed': ['40'], 'inner_wall_speed': ['70'],
        'initial_layer_speed': ['25'], 'enable_arc_fitting': '0',
        'elefant_foot_compensation': '0.15'})}

    def mesh(name):
        result = trimesh.load_mesh(HERE / (name + '.stl'))
        if not result.is_watertight or len(result.split()) != 1:
            raise ValueError('Invalid panel mesh')
        return result

    module.mesh = mesh
    module.main([JOB])
    intent = {'printer': 'Bambu Lab X1 Carbon 0.4 nozzle', 'plate': 'Textured PEI Plate',
              'bed_c': 55, 'nozzle_c': 220, 'layer_height_mm': .1,
              'parts': [{'name': PART, 'holes': 0, 'centre_mm': [128, 128],
                         'source_sha256': hashlib.sha256((HERE / (PART + '.coscad')).read_bytes()).hexdigest(),
                         'stl_sha256': validation['stl_sha256']}]}
    (FOLDER / 'print-intent.json').write_text(json.dumps(intent, indent=2) + '\n')
    for name in ('process', 'project', 'filament'):
        path = FOLDER / 'presets' / (name + '.json')
        settings = json.loads(path.read_text())
        if name in ('process', 'project'):
            settings.update(layer_height='0.1', initial_layer_print_height='0.2')
        if name in ('filament', 'project'):
            settings.update(textured_plate_temp=['55'], textured_plate_temp_initial_layer=['55'],
                            nozzle_temperature=['220'], nozzle_temperature_initial_layer=['220'])
        path.write_text(json.dumps(settings, indent=2) + '\n')
    project = FOLDER / 'tank-project.3mf'
    with zipfile.ZipFile(project) as archive:
        entries = {e.filename: archive.read(e) for e in archive.infolist()}
    entries['Metadata/project_settings.config'] = (FOLDER / 'presets/project.json').read_bytes()
    with zipfile.ZipFile(project, 'w', zipfile.ZIP_DEFLATED) as archive:
        for name, data in entries.items():
            archive.writestr(name, data)
    output = FOLDER / 'slice'
    output.mkdir(exist_ok=True)
    command = [module.s.APP, '--debug', '2', '--load-settings',
               str(FOLDER / 'presets/machine.json') + ';' + str(FOLDER / 'presets/process.json'),
               '--load-filaments', str(FOLDER / 'presets/filament.json'), '--arrange', '0', '--orient', '0',
               '--slice', '1', '--export-3mf', 'tank.gcode.3mf', '--outputdir', str(output), str(project)]
    (output / 'slice-command.json').write_text(json.dumps(command, indent=2) + '\n')
    with (output / 'slicer.log').open('w') as log:
        subprocess.run(command, stdout=log, stderr=log, check=True)
    print('Sliced', JOB, flush=True)


if __name__ == '__main__':
    main()
