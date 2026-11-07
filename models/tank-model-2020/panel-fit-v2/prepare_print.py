#!/usr/bin/env python3
"""Prepare and slice only the three panel coupons on a central X1C plate."""
import importlib.util
import hashlib
import json
from pathlib import Path
import subprocess
import sys

import trimesh

HERE = Path(__file__).resolve().parent
JOB = 'x1c-panel-fit-20261107'
FOLDER = HERE / JOB
PARTS = [f'panel-c{c:03d}-{holes}holes' for holes, c in enumerate((20, 25, 30), 1)]


def main(here=HERE, job=JOB, parts=PARTS):
    HERE, JOB, PARTS = Path(here), job, parts
    FOLDER = HERE / JOB
    if (FOLDER / 'print-run.json').exists():
        raise RuntimeError('An existing dispatch record prevents rebuilding this job')
    spec = importlib.util.spec_from_file_location('tank_prepare', HERE.parent / 'prepare_slice.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.B = HERE
    module.JOBS = {JOB: (list(zip(PARTS, [(100, 128), (128, 128), (156, 128)])), {
        **module.COMMON, 'wall_loops': '4', 'sparse_infill_density': '100%',
        'top_shell_layers': '5', 'bottom_shell_layers': '5',
        'brim_type': 'no_brim', 'brim_width': '0', 'print_sequence': 'by layer',
        'outer_wall_speed': ['40'], 'inner_wall_speed': ['70'],
        'initial_layer_speed': ['25'], 'layer_height': '0.1',
        'enable_arc_fitting': '0', 'elefant_foot_compensation': '0.15'})}

    def mesh(name):
        result = trimesh.load_mesh(HERE / f'{name}.stl')
        if not result.is_watertight or len(result.split()) != 1:
            raise ValueError(f'Invalid coupon: {name}')
        result.apply_translation([-result.bounds[:, 0].mean(), -result.bounds[:, 1].mean(), -result.bounds[0, 2]])
        return result

    module.mesh = mesh
    module.main([JOB])
    intent = {'printer': 'Bambu Lab X1 Carbon 0.4 nozzle', 'plate': 'Textured PEI Plate',
              'bed_c': 55, 'nozzle_c': 220, 'layer_height_mm': .1,
              'parts': [{'name': name, 'holes': holes, 'centre_mm': [x, 128],
                         'source_sha256': hashlib.sha256((HERE / f'{name}.coscad').read_bytes()).hexdigest(),
                         'stl_sha256': hashlib.sha256((HERE / f'{name}.stl').read_bytes()).hexdigest()}
                        for holes, (name, x) in enumerate(zip(PARTS, (100, 128, 156)), 1)]}
    (FOLDER / 'print-intent.json').write_text(json.dumps(intent, indent=2) + '\n')
    # The existing tank builder chooses 0.2 mm layers after merging overrides.
    # Set both the explicit process input and project copy to the precision intent.
    for name in ('process', 'project'):
        path = FOLDER / 'presets' / f'{name}.json'
        settings = json.loads(path.read_text())
        settings.update(layer_height='0.1', initial_layer_print_height='0.2')
        path.write_text(json.dumps(settings, indent=2) + '\n')
    for name in ('filament', 'project'):
        path = FOLDER / 'presets' / f'{name}.json'
        settings = json.loads(path.read_text())
        settings.update(textured_plate_temp=['55'], textured_plate_temp_initial_layer=['55'],
                        nozzle_temperature=['220'], nozzle_temperature_initial_layer=['220'])
        path.write_text(json.dumps(settings, indent=2) + '\n')
    # Refresh the embedded project settings using the same independently saved inputs.
    import zipfile
    project = FOLDER / 'tank-project.3mf'
    with zipfile.ZipFile(project) as source:
        entries = {entry.filename: source.read(entry) for entry in source.infolist()}
    entries['Metadata/project_settings.config'] = (FOLDER / 'presets/project.json').read_bytes()
    with zipfile.ZipFile(project, 'w', zipfile.ZIP_DEFLATED) as output:
        for name, data in entries.items():
            output.writestr(name, data)
    out = FOLDER / 'slice'
    out.mkdir(exist_ok=True)
    command = [module.s.APP, '--debug', '2', '--load-settings',
               str(FOLDER / 'presets/machine.json') + ';' + str(FOLDER / 'presets/process.json'),
               '--load-filaments', str(FOLDER / 'presets/filament.json'),
               '--arrange', '0', '--orient', '0', '--slice', '1',
               '--export-3mf', 'tank.gcode.3mf', '--outputdir', str(out), str(project)]
    (out / 'slice-command.json').write_text(json.dumps(command, indent=2) + '\n')
    with (out / 'slicer.log').open('w') as log:
        subprocess.run(command, stdout=log, stderr=log, check=True)
    print('Sliced', JOB, flush=True)


if __name__ == '__main__':
    main()
