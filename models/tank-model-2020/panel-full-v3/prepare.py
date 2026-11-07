#!/usr/bin/env python3
"""Build and geometrically verify one complete bilateral wall panel; no printer access."""
import hashlib
import json
from pathlib import Path
import re
import subprocess
import tempfile

import manifold3d as md
import numpy as np
import trimesh

HERE = Path(__file__).resolve().parent
BASE = HERE.parent
COSCAD = '/opt/homebrew/bin/coscad'


def require(condition, reason):
    if not condition:
        raise ValueError(reason)


def render(source):
    subprocess.run([COSCAD, 'stl', str(source), '-o', str(source.with_suffix('.stl'))], check=True)
    mesh = trimesh.load_mesh(source.with_suffix('.stl'))
    require(mesh.is_watertight and mesh.is_winding_consistent and mesh.volume > 0 and len(mesh.split()) == 1, source.name)
    solid = md.Manifold(md.Mesh(np.asarray(mesh.vertices, np.float32), np.asarray(mesh.faces, np.uint32)))
    require(solid.status() == md.Error.NoError, str(solid.status()))
    return mesh, solid


def main():
    if any(HERE.glob('x1c-*/print-run.json')):
        raise RuntimeError('Existing dispatch prevents rebuilding this trial')
    source = (BASE / 'panel_wall.coscad').read_text()
    # Preserve the successful coupon sources by leaving the old one-sided model intact.
    # Mirror exactly the tested stepped profile; use the V3 coupon's 3 mm ramps.
    for key, value in [('clearance', '0.25'), ('key_length', '59.4'), ('lead', '3')]:
        source, count = re.subn(rf'^{key} = .*$', f'{key} = {value}', source, flags=re.M)
        require(count == 1, f'Expected one {key} definition')
    source = source.replace('main =', 'one_sided =')
    source += '''
// Full 70 x 70 mm wall, with the tested 0.25 mm bilateral T-heads.
// Keys stop 5.3 mm short of each corner; 3 mm ramps need no supports upright.
panel = one_sided |> add (⇋ 0 0 1 one_sided)
upright = panel |> rotx 90 |> z 35
// Four 0.2 mm mouse ears brace the plain bottom tongue; trim them flush before fitting.
// They stay away from the left/right keys so no mating shoulder is altered.
ears = (zcyl 4 0.2 |> move -22 -4 0.1)
  |> add (zcyl 4 0.2 |> move -22 4 0.1)
  |> add (zcyl 4 0.2 |> move 22 -4 0.1)
  |> add (zcyl 4 0.2 |> move 22 4 0.1)
main = upright |> add ears
'''
    # Remove inherited comments about one-sided flat printing from this standalone revision.
    source = source.replace('// The -Z face stays flat on the print bed: no underside\n// T-heads, supports or trapped support material in these sliding faces.\n', '')
    source = source.replace('// every shoulder. The -Z face stays flat on the print bed: no underside\n// T-heads, supports or trapped support material in these sliding faces.', '// every shoulder; this intermediate is reflected below.')
    path = HERE / 'panel-wall-double-c025-upright.coscad'
    path.write_text(source)
    mesh, _ = render(path)
    require(np.allclose(mesh.bounds[:, [0, 2]], [[-35, 0], [35, 70]], atol=1e-4)
            and 7.98 < mesh.bounds[1, 1] <= 8.001
            and -8.001 <= mesh.bounds[0, 1] < -7.98, 'Wrong full-panel dimensions/pose')
    hanging = (mesh.face_normals[:, 2] < -np.sqrt(.5) - 1e-4) & (mesh.triangles[:, :, 2].max(axis=1) > 1e-4)
    require(not hanging.any(), 'Unsupported fitting face above 45 degrees')
    with tempfile.TemporaryDirectory(prefix='tank-full-panel-') as tmp:
        tmp = Path(tmp)
        bare = tmp / 'bare.coscad'
        bare.write_text(source.rsplit('main =', 1)[0] + 'main = upright\n')
        bare_mesh, panel = render(bare)
        require(np.allclose(bare_mesh.extents, [70, 10.5, 70], atol=1e-4), 'Wrong neck/head dimensions')
        rail_path = tmp / 'rail.coscad'
        rail_path.write_text((BASE / 'rail100.coscad').read_text())
        _, rail = render(rail_path)
        uprights = rail.translate((-40, 0, 50)) + rail.translate((40, 0, 50))
        seated = panel.translate((0, 0, 15))
        offsets = np.arange(0, 100.001, .25)
        max_overlap = max(abs((seated.translate((0, 0, float(d))) ^ uprights).volume()) for d in offsets)
        require(max_overlap < 1e-5, 'Full panel cannot slide between both uprights')
        horizontal = rail.trim_by_plane((0, 0, 1), -30).trim_by_plane((0, 0, -1), -30).rotate((0, 90, 0))
        require(abs(horizontal.volume() - .6 * rail.volume()) < 1e-4, 'Incorrect horizontal rail length')
        assembled = uprights + horizontal.translate((0, 0, 10)) + horizontal.translate((0, 0, 90))
        final_overlap = abs((seated ^ assembled).volume())
        require(final_overlap < 1e-5, 'Final panel collides with top/bottom rails')
        overtravel = [abs((seated.translate((0, sign * .35, 0)) ^ uprights).volume()) for sign in (-1, 1)]
        require(min(overtravel) > .1, 'Panel is not captured on both faces')
    report = {'passed': True, 'clearance_per_face_mm': .25, 'panel_size_mm': [70, 70],
              'neck_mm': 5.7, 'head_width_mm': 10.5, 'key_length_mm': 59.4, 'ramp_mm': 3,
              'source_sha256': hashlib.sha256(path.read_bytes()).hexdigest(),
              'stl_sha256': hashlib.sha256(path.with_suffix('.stl').read_bytes()).hexdigest(),
              'compiler': subprocess.check_output([COSCAD, '--version'], text=True).strip(),
              'compiler_sha256': hashlib.sha256(Path(COSCAD).read_bytes()).hexdigest(),
              'source_rail_sha256': hashlib.sha256((BASE / 'rail100.coscad').read_bytes()).hexdigest(),
              'source_one_sided_wall_sha256': hashlib.sha256((BASE / 'panel_wall.coscad').read_bytes()).hexdigest(),
              'bounds_mm': mesh.bounds.tolist(), 'volume_mm3': mesh.volume,
              'watertight_single_component': True, 'unsupported_downward_faces_over_45deg': int(hanging.sum()),
              'sliding_sweep_samples': len(offsets), 'sliding_sweep_max_overlap_mm3': max_overlap,
              'assembled_four_rail_overlap_mm3': final_overlap, 'normal_overtravel_collision_mm3': overtravel,
              'full_panel_physical_fit_tested': False, 'waterproofing_tested': False}
    (HERE / 'geometry-validation.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
