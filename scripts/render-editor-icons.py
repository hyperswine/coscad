#!/usr/bin/env python3
"""Bake native CoScad meshes into transparent, shaded SVG primitive symbols.

Development-only: numpy/trimesh; the extension uses only the bundled SVGs.
The camera, scale and lighting are shared across icons; no source is run by VS Code.
"""
import argparse
import html
import json
from pathlib import Path
import subprocess
import tempfile

import numpy as np
import trimesh

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / 'vscode' / 'assets' / 'shapes'


def render(mesh, title, axis):
    eye = np.array([5., -7., 5.])
    eye /= np.linalg.norm(eye)
    right = np.cross(-eye, [0., 0., 1.]); right /= np.linalg.norm(right)
    up = np.cross(right, -eye)
    vertices = mesh.vertices - mesh.bounds.mean(axis=0)
    projected = np.column_stack((vertices @ right, -(vertices @ up)))
    extent = np.ptp(projected, axis=0)
    scale = 100 / max(extent)
    projected = (projected - (projected.min(axis=0) + projected.max(axis=0)) / 2) * scale + [64, 64]
    light = np.array([-3., -5., 8.]); light /= np.linalg.norm(light)
    normals = mesh.face_normals
    visible = np.flatnonzero(normals @ eye > 1e-7)
    depths = mesh.triangles_center @ eye
    faces = sorted(visible, key=lambda i: depths[i])
    output = [f'<svg xmlns="http://www.w3.org/2000/svg" width="128" height="128" viewBox="0 0 128 128" role="img"><title>{html.escape(title)}</title>']
    for i in faces:
        brightness = .53 + .47 * max(0, float(normals[i] @ light))
        rgb = tuple(round(value * brightness) for value in (250, 185, 64))
        color = '#%02x%02x%02x' % rgb
        points = ' '.join(f'{x:.2f},{y:.2f}' for x, y in projected[mesh.faces[i]])
        output.append(f'<polygon points="{points}" fill="{color}" stroke="{color}" stroke-width="0.35" stroke-linejoin="round"/>')
    if axis:
        color = {'X': '#dc5252', 'Y': '#39a66d', 'Z': '#4b8ce8'}[axis]
        output.append(f'<rect x="99" y="99" width="26" height="26" rx="8" fill="{color}"/><text x="112" y="118" text-anchor="middle" font-family="system-ui,sans-serif" font-size="20" font-weight="700" fill="white">{axis}</text>')
    return '\n'.join([*output, '</svg>', ''])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--coscad', required=True)
    args = parser.parse_args()
    OUT.mkdir(parents=True, exist_ok=True)
    catalog = json.loads((ROOT / 'vscode' / 'shape-icons.json').read_text())
    with tempfile.TemporaryDirectory(prefix='coscad-editor-icons-') as temporary:
        work = Path(temporary)
        for item in catalog:
            source = work / (item['id'] + '.coscad')
            source.write_text('main = ' + item['source'] + '\n')
            subprocess.run([args.coscad, 'stl', str(source), '-o', str(source.with_suffix('.stl'))], check=True, capture_output=True)
            mesh = trimesh.load_mesh(source.with_suffix('.stl'))
            (OUT / (item['id'] + '.svg')).write_text(render(mesh, item['title'], item.get('axis')))
    print(f'Rendered {len(catalog)} bundled shape symbols')


if __name__ == '__main__':
    main()
