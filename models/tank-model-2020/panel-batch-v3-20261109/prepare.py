#!/usr/bin/env python3
"""Preserve the successful wall; make and verify a matching notched floor. No printer access."""
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import tempfile

import manifold3d as md
import numpy as np
import trimesh

HERE = Path(__file__).resolve().parent
BASE = HERE.parent
PRIOR = BASE / 'panel-full-v3'
COSCAD = str(BASE.parents[1] / 'bin/coscad')
WALL = 'panel-wall-double-c025-upright'
FLOOR = 'panel-floor-double-c025-diagonal'


def require(condition, reason):
    if not condition:
        raise ValueError(reason)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def solid(path):
    mesh = trimesh.load_mesh(path)
    require(mesh.is_watertight and mesh.is_winding_consistent and mesh.volume > 0 and len(mesh.split()) == 1, path.name)
    result = md.Manifold(md.Mesh(np.asarray(mesh.vertices, np.float32), np.asarray(mesh.faces, np.uint32)))
    require(result.status() == md.Error.NoError, str(result.status()))
    return mesh, result


def render(path):
    subprocess.run([COSCAD, 'stl', str(path), '-o', str(path.with_suffix('.stl'))], check=True)
    return solid(path.with_suffix('.stl'))


def main():
    require(not any(HERE.glob('x1c-*/print-run.json')), 'Existing dispatch prevents rebuilding this batch')
    prior = json.loads((PRIOR / 'geometry-validation.json').read_text())
    require(prior['passed'] and digest(PRIOR / (WALL + '.coscad')) == prior['source_sha256'], 'Successful wall source changed')
    require(digest(PRIOR / (WALL + '.stl')) == prior['stl_sha256'], 'Successful wall mesh changed')
    for suffix in ('.coscad', '.stl'):
        shutil.copy2(PRIOR / (WALL + suffix), HERE / (WALL + suffix))
    wall_mesh, _ = solid(HERE / (WALL + '.stl'))
    common = (PRIOR / (WALL + '.coscad')).read_text().split('upright =', 1)[0]
    source = common.replace('// Sliding wall panel,', '// Floor panel,').replace('// Full 70 x 70 mm wall,', '// Full 70 x 70 mm floor,')
    source += '''
// Original 5.3 mm corner notches clear all four full-height uprights.
corner_notched = panel
  |> cut (box 10.6 10.6 13 |> move 35 35 0)
  |> cut (box 10.6 10.6 13 |> move -35 35 0)
  |> cut (box 10.6 10.6 13 |> move 35 -35 0)
  |> cut (box 10.6 10.6 13 |> move -35 -35 0)
// Tilt in its own plane: notch ledges are now 45 degrees, not flat cantilevers.
raw_height = 64.7 * 1.4142135623730951
ear_x = 5.3 / 1.4142135623730951 + 1
raw_upright = corner_notched |> rotz 45 |> rotx 90 |> z (raw_height / 2)
// Blunt the four needle-thin notch tips by 0.249809 mm vertically so the
// first and final layers have at least a 0.499617 mm-wide printable land.
// This removes under 2 cubic mm; it does not change nominal slot clearance.
tip_trim = (raw_height - 91) / 2
upright = (raw_upright |> isect (box 110 20 91 |> z (raw_height / 2))) |> z (-tip_trim)
// Same physical floor in the assembled orientation, used by the fit audit.
notched = upright |> z -45.5 |> rotx -90 |> rotz -45
// Removable 0.20 mm mouse ears brace each bottom contact; keep the notch open.
ears = (zcyl 4 0.2 |> move (-ear_x) -4 0.1)
  |> add (zcyl 4 0.2 |> move (-ear_x) 4 0.1)
  |> add (zcyl 4 0.2 |> move ear_x -4 0.1)
  |> add (zcyl 4 0.2 |> move ear_x 4 0.1)
main = upright |> add ears
'''
    path = HERE / (FLOOR + '.coscad')
    path.write_text(source)
    floor_mesh, floor_print = render(path)
    for name, mesh in ((WALL, wall_mesh), (FLOOR, floor_mesh)):
        require(mesh.euler_number == 2, f'Unexpected holes: {name}')
        require(abs(mesh.bounds[0, 2]) < 1e-5, f'Part is above/below bed: {name}')
        hanging = (mesh.face_normals[:, 2] < -np.sqrt(.5) - 1e-4) & (mesh.triangles[:, :, 2].max(axis=1) > 1e-4)
        require(not hanging.any(), f'Unsupported fitting surface: {name}')
    height = 64.7 * np.sqrt(2)
    require(np.allclose(floor_mesh.extents, [height, wall_mesh.extents[1], 91], atol=1e-4), 'Wrong floor pose')
    with tempfile.TemporaryDirectory(prefix='tank-panel-batch-') as tmp:
        tmp = Path(tmp)
        bare = tmp / 'floor.coscad'
        bare.write_text(source.rsplit('main =', 1)[0] + 'main = notched\n')
        mesh, floor = render(bare)
        require(np.allclose(mesh.extents, [70, 70, 10.5], atol=1e-4), 'Wrong floor dimensions')
        wall = tmp / 'wall.coscad'
        wall.write_text(common + '\nmain = panel\n')
        bare_wall_mesh, _ = render(wall)
        raw = tmp / 'raw-floor.coscad'
        raw.write_text(source.rsplit('main =', 1)[0] + 'main = corner_notched\n')
        raw_mesh, _ = render(raw)
        require(abs(bare_wall_mesh.volume - raw_mesh.volume - 4 * 5.3**2 * 5.7) < .001, 'Corner notch volume differs')
        tip_volume = raw_mesh.volume - mesh.volume
        require(0 < tip_volume < 2, 'Unexpected material removed by tip trims')
        probes = []
        for x in (-32, 32):
            for y in (-32, 32):
                overlap = abs((floor ^ md.Manifold.cube((4, 4, 10), center=True).translate((x, y, 0))).volume())
                require(overlap < 1e-5, 'Missing corner notch')
                probes.append(overlap)
        rail_path = tmp / 'rail.coscad'
        rail_path.write_text((BASE / 'rail100.coscad').read_text())
        _, upright = render(rail_path)
        rail = upright.trim_by_plane((0, 0, 1), -30).trim_by_plane((0, 0, -1), -30)
        require(abs(rail.volume() - .6 * upright.volume()) < 1e-4, 'Wrong 60 mm rail')
        front = rail.rotate((0, 90, 0)).translate((0, -40, 0))
        back = rail.rotate((0, 90, 0)).translate((0, 40, 0))
        sides = [rail.rotate((90, 0, 0)).translate((x, 0, 0)) for x in (-40, 40)]
        uprights = [upright.translate((x, y, 40)) for x in (-40, 40) for y in (-40, 40)]
        overlaps = [abs((floor ^ fixed).volume()) for fixed in (front, back, *sides, *uprights)]
        require(max(overlaps) < 1e-5, 'Floor overlaps assembled rails/uprights')
        maximum, samples = 0., 0
        for offset in np.arange(0, 100.001, .25):
            maximum = max(maximum, abs((floor.translate((0, float(offset), 0)) ^ front).volume()))
            for side in sides:
                maximum = max(maximum, abs((side.translate((0, float(offset), 0)) ^ floor).volume()))
            samples += 3
        require(maximum < 1e-5, 'Blocked floor/front or side-rail insertion')
        controls = [abs((floor.translate((0, 0, sign * .35)) ^ sides[0]).volume()) for sign in (-1, 1)]
        require(min(controls) > .1, 'Missing bilateral floor capture')
    report = {'passed': True, 'wall_matches_successful_source_and_mesh': True,
              'clearance_per_face_mm': .25, 'notches_mm': [5.3, 5.3], 'notches': 4,
              'floor_bounds_mm': floor_mesh.bounds.tolist(), 'floor_unsupported_faces_over_45deg': 0,
              'corner_probe_intersection_mm3': probes, 'assembled_intersection_mm3': overlaps,
              'tip_trim_print_z_mm': (height - 91) / 2, 'tip_trim_total_mm3': tip_volume,
              'insertion_sweep_samples': samples, 'insertion_sweep_max_intersection_mm3': maximum,
              'overtravel_collision_mm3': controls, 'floor_physical_fit_tested': False,
              'compiler': subprocess.check_output([COSCAD, '--version'], text=True).strip(),
              'compiler_sha256': digest(Path(COSCAD)), 'rail_source_sha256': digest(BASE / 'rail100.coscad'),
              'parts': {name: {'source_sha256': digest(HERE / (name + '.coscad')), 'stl_sha256': digest(HERE / (name + '.stl'))}
                        for name in (WALL, FLOOR)}}
    (HERE / 'geometry-validation.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
