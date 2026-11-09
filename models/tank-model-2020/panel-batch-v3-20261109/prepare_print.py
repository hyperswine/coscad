#!/usr/bin/env python3
"""Slice three unchanged successful walls and one notched floor, using saved successful presets."""
import hashlib
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import zipfile

import trimesh

HERE = Path(__file__).resolve().parent
JOB = 'x1c-three-walls-notched-floor-20261109'
FOLDER = HERE / JOB
WALL = 'panel-wall-double-c025-upright'
FLOOR = 'panel-floor-double-c025-diagonal'
LAYOUT = [(WALL, (82, 75)), (WALL, (82, 128)), (WALL, (82, 181)), (FLOOR, (176, 128))]
REFERENCE = HERE.parent / 'panel-full-v3/x1c-full-panel-c025-20261107/presets'


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    if (FOLDER / 'print-run.json').exists():
        raise RuntimeError('Existing dispatch prevents rebuilding this job')
    report = json.loads((HERE / 'geometry-validation.json').read_text())
    if not report['passed']:
        raise RuntimeError('Geometry not validated')
    for part in (WALL, FLOOR):
        for suffix, key in (('.coscad', 'source_sha256'), ('.stl', 'stl_sha256')):
            if digest(HERE / (part + suffix)) != report['parts'][part][key]:
                raise RuntimeError('Source/mesh changed after geometry validation')
    spec = importlib.util.spec_from_file_location('tank_prepare', HERE.parent / 'prepare_slice.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.B = HERE
    module.JOBS = {JOB: (LAYOUT, {'wall_loops': '4', 'sparse_infill_density': '100%',
                                'brim_type': 'no_brim', 'brim_width': '0'})}

    def mesh(name):
        item = trimesh.load_mesh(HERE / (name + '.stl'))
        if not item.is_watertight or len(item.split()) != 1:
            raise ValueError('Invalid part mesh')
        item.apply_translation([-item.bounds[:, 0].mean(), -item.bounds[:, 1].mean(), -item.bounds[0, 2]])
        return item

    module.mesh = mesh
    module.main([JOB])
    # Restore all four inputs byte-for-byte, including startup templates and speeds,
    # from the panel the owner actually printed and fitted successfully.
    reference_hashes = {}
    for name in ('machine', 'process', 'filament', 'project'):
        source = REFERENCE / (name + '.json')
        shutil.copy2(source, FOLDER / 'presets' / source.name)
        reference_hashes[source.name] = digest(source)
    intent = {'printer': 'Bambu Lab X1 Carbon 0.4 nozzle', 'plate': 'Textured PEI Plate',
              'bed_c': 55, 'nozzle_c': 220, 'layer_height_mm': .1,
              'reference_presets_sha256': reference_hashes,
              'parts': [{'name': name, 'object_id': 2 * index + 2, 'holes': 0, 'centre_mm': list(position),
                         **report['parts'][name]} for index, (name, position) in enumerate(LAYOUT)]}
    (FOLDER / 'print-intent.json').write_text(json.dumps(intent, indent=2) + '\n')
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
