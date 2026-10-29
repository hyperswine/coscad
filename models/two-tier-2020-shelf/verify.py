#!/usr/bin/env python3
"""Independent mesh, hardware-path and packing checks; make assembly previews.

Requires numpy/scipy/trimesh/manifold3d. This does not operate printers or
need OpenSCAD: parts render with `coscad stl`, booleans use manifold3d and the
previews are drawn by a small depth-buffered rasterizer in this file.
"""
import argparse
import hashlib
import json
from pathlib import Path
import struct
import subprocess
import zlib
import numpy as np
from scipy.spatial.transform import Rotation
import trimesh

HERE = Path(__file__).resolve().parent
EVIDENCE = HERE / 'evidence'


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--coscad', required=True)
    ap.add_argument('--evidence', type=Path, default=EVIDENCE,
                    help='where part meshes and geometry.json go (default evidence/)')
    ap.add_argument('--images', type=Path, default=HERE,
                    help='where shelf-render.png and details.png go (default this folder)')
    args = ap.parse_args()
    evidence = args.evidence
    evidence.mkdir(parents=True, exist_ok=True)
    design = json.loads((HERE/'model.json').read_text())
    meshes, report = {}, {'parts': {}, 'hardware_paths': 0, 'physical_fit_verified': False,
                          'physical_load_tested': False, 'dispatched': False}
    for name in design['parts']:
        source = HERE/'parts'/f'{name}.coscad'
        result = subprocess.run([args.coscad, 'stl', str(source), '-o', str(evidence/f'{name}.stl')], capture_output=True, text=True)
        if result.returncode:
            raise RuntimeError(result.stdout+result.stderr)
        mesh = trimesh.load_mesh(evidence/f'{name}.stl')
        components = mesh.split()
        assert mesh.is_watertight and mesh.is_winding_consistent and mesh.volume > 0 and len(components) == 1, name
        meshes[name] = mesh
        report['parts'][name] = dict(watertight=True, components=1, volume_mm3=mesh.volume,
                                     bounds_mm=mesh.bounds.tolist(), source_sha256=hashlib.sha256(source.read_bytes()).hexdigest())
    # Canonical assembled rails and print sources must represent the same solid.
    for length in (50, 160, 170, 230):
        a = meshes[f'rail_{length}'].copy()
        b = meshes[f'rail_print_{length}']
        common = trimesh.boolean.intersection([a,b], engine='manifold').volume
        assert abs(a.volume+b.volume-2*common) < 0.01
    world = {}
    for item in design['instances']:
        transform = np.eye(4)
        transform[:3,:3] = Rotation.from_euler('xyz', item['rotation_xyz_deg'], degrees=True).as_matrix()
        transform[:3,3] = item['position_mm']
        world[item['id']] = meshes[item['part']].copy().apply_transform(transform)
    normals = {'top':[0,0,1], 'bot':[0,0,-1], 'fwd':[0,-1,0], 'bak':[0,1,0], 'lft':[-1,0,0], 'rt':[1,0,0]}
    for bolt in design['hardware']:
        normal = np.array(normals[bolt['face']])
        origin = np.array(bolt['hole_world_mm'])
        is_panel = bolt['part'].startswith('panel_')
        bearing = origin if is_panel else origin+3*normal
        shank = trimesh.creation.cylinder(radius=2.5, height=12, sections=32)
        transform = trimesh.geometry.align_vectors([0,0,1], normal)
        transform[:3,3] = bearing-6*normal
        shank.apply_transform(transform)
        for owner in (bolt['part'], bolt['host']):
            common = trimesh.boolean.intersection([shank,world[owner]], engine='manifold')
            assert common.volume < 0.01, (owner, bolt, common.volume)
        report['hardware_paths'] += 1
    # Provisional rectangular sliding-nut envelope; actual nut dimensions unknown.
    nut = trimesh.creation.box([10,9.6,2.6])
    nut.apply_translation([0,0,7])
    common = trimesh.boolean.intersection([nut, meshes['rail_50']], engine='manifold')
    assert common.volume < 0.001
    report['nominal_nut_envelope_mm'] = [10,9.6,2.6]
    report['nut_envelope_interference_mm3'] = common.volume
    # Independently check transformed assembly rather than relying on bounding boxes.
    overlaps = []
    items = list(world.items())
    for j, (an, a) in enumerate(items):
        for bn, b in items[j+1:]:
            extent = np.minimum(a.bounds[1],b.bounds[1])-np.maximum(a.bounds[0],b.bounds[0])
            if np.any(extent < 1e-5):
                continue
            volume = trimesh.boolean.intersection([a,b], engine='manifold').volume
            if volume > 0.02:
                overlaps.append([an,bn,volume])
    assert not overlaps, overlaps
    report['assembly_overlaps'] = overlaps
    for stem in ('manufacture','fit'):
        manifest = json.loads((HERE/f'{stem}_manifest.json').read_text())
        variants = {v['id']:v for v in manifest['variants']}
        total = 0
        for bed in manifest['beds']:
            actual = trimesh.load_mesh(HERE/Path(bed['stl']).name)
            assert actual.is_watertight and len(actual.split()) == len(bed['placements'])
            # A ribbed panel must start on its broad flat skin, not its rib grid.
            if all(p['variant'].startswith('panel_') for p in bed['placements']):
                on_bed = np.all(np.abs(actual.triangles[:,:,2]) < 1e-4,axis=1)
                downward = actual.face_normals[:,2] < -.99
                assert actual.area_faces[on_bed & downward].sum() > 35000
            occupied = []
            for placement in bed['placements']:
                b = placement['brim_mm']
                x,y,w,d = (placement[k] for k in ('x','y','w','d'))
                bounds = [x-b,y-b,x+w+b,y+d+b]
                assert min(bounds[:2]) >= 6-0.001 and max(bounds[2:]) <= 250.001, bounds
                assert variants[placement['variant']]['footprint'][2] <= 250
                for other in occupied:
                    assert bounds[2] <= other[0]+0.001 or other[2] <= bounds[0]+0.001 or bounds[3] <= other[1]+0.001 or other[3] <= bounds[1]+0.001
                occupied.append(bounds)
                total += 1
        assert total == (48 if stem == 'manufacture' else 3), total
        report[stem] = dict(beds=len(manifest['beds']), parts=total, brim_included=True)
    args.images.mkdir(parents=True, exist_ok=True)
    render(design, world, meshes, args.images)
    (evidence/'geometry.json').write_text(json.dumps(report, indent=2)+'\n')
    print(json.dumps({k:v for k,v in report.items() if k!='parts'}, indent=2))


COLORS = {'rail': [.23, .34, .43], 'post': [.23, .34, .43],
          'connector': [.28, .55, .51], 'panel': [.88, .71, .40]}


def render(design, world, meshes, out):
    # Depth-buffered pictures of the verified native meshes at their poses.
    # Matplotlib's painter ordering is unreliable for slotted, perforated solids.
    # Hardware omitted.
    rasterize([(world[item['id']], COLORS[item['kind']]) for item in design['instances']],
              out/'shelf-render.png', (1400, 1000), azimuth=25, elevation=35)
    # L plate, centre T plate and panel underside.
    flip = np.eye(4)
    flip[:3, :3] = Rotation.from_euler('xyz', [180, 0, 0], degrees=True).as_matrix()
    flip[:3, 3] = [95, 0, 11]
    details = [(meshes['gusset_l'].copy().apply_translation([-240, 0, 0]), COLORS['connector']),
               (meshes['gusset_t'].copy().apply_translation([-110, 0, 0]), COLORS['connector']),
               (meshes['panel_left'].copy().apply_transform(flip), COLORS['panel'])]
    rasterize(details, out/'details.png', (1600, 700), azimuth=0, elevation=75, fill=0.55)


def rasterize(items, path, size, azimuth, elevation, fill=0.7):
    """Orthographic z-buffer render of (mesh, rgb) pairs to an RGB PNG."""
    width, height = size
    az, el = np.radians(azimuth), np.radians(elevation)
    eye = np.array([np.cos(el)*np.sin(az), -np.cos(el)*np.cos(az), np.sin(el)])
    right = np.cross(-eye, [0, 0, 1])
    right /= np.linalg.norm(right)
    up = np.cross(right, -eye)
    light = eye + 0.6*right + 0.8*up
    light /= np.linalg.norm(light)
    tris = np.concatenate([m.triangles for m, _ in items])
    normals = np.concatenate([m.face_normals for m, _ in items])
    colors = np.concatenate([np.tile(c, (len(m.faces), 1)) for m, c in items])
    front = normals @ eye > 1e-9
    tris, normals, colors = tris[front], normals[front], colors[front]
    shade = colors * (0.45 + 0.55*np.clip(normals @ light, 0, 1))[:, None]
    screen = np.stack([tris @ right, tris @ up, tris @ eye], axis=-1)
    lo, hi = screen[..., :2].reshape(-1, 2).min(0), screen[..., :2].reshape(-1, 2).max(0)
    scale = fill * min(width/(hi[0]-lo[0]), height/(hi[1]-lo[1]))
    centre = (lo+hi)/2
    px = (screen[..., 0]-centre[0])*scale + width/2
    py = height/2 - (screen[..., 1]-centre[1])*scale
    depth = np.full((height, width), -np.inf)
    image = np.full((height, width, 3), 0.973)
    for k in range(len(tris)):
        xs, ys, zs = px[k], py[k], screen[k, :, 2]
        x0, x1 = max(int(np.floor(xs.min())), 0), min(int(np.ceil(xs.max())), width-1)
        y0, y1 = max(int(np.floor(ys.min())), 0), min(int(np.ceil(ys.max())), height-1)
        if x0 > x1 or y0 > y1:
            continue
        gx, gy = np.meshgrid(np.arange(x0, x1+1)+0.5, np.arange(y0, y1+1)+0.5)
        area = (xs[1]-xs[0])*(ys[2]-ys[0]) - (xs[2]-xs[0])*(ys[1]-ys[0])
        if abs(area) < 1e-12:
            continue
        w1 = ((gx-xs[0])*(ys[2]-ys[0]) - (xs[2]-xs[0])*(gy-ys[0])) / area
        w2 = ((xs[1]-xs[0])*(gy-ys[0]) - (gx-xs[0])*(ys[1]-ys[0])) / area
        w0 = 1-w1-w2
        inside = (w0 >= -1e-6) & (w1 >= -1e-6) & (w2 >= -1e-6)
        z = w0*zs[0] + w1*zs[1] + w2*zs[2]
        block = depth[y0:y1+1, x0:x1+1]
        nearer = inside & (z > block)
        block[nearer] = z[nearer]
        image[y0:y1+1, x0:x1+1][nearer] = shade[k]
    write_png(path, (np.clip(image, 0, 1)*255).astype(np.uint8))


def write_png(path, pixels):
    height, width, _ = pixels.shape
    raw = b''.join(b'\x00' + pixels[row].tobytes() for row in range(height))
    def chunk(kind, data):
        return struct.pack('>I', len(data)) + kind + data + struct.pack('>I', zlib.crc32(kind + data))
    Path(path).write_bytes(b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('>IIBBBBB', width, height, 8, 2, 0, 0, 0))
                           + chunk(b'IDAT', zlib.compress(raw, 9)) + chunk(b'IEND', b''))


if __name__ == '__main__':
    main()
