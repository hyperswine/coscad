#!/usr/bin/env python3
"""Generate the PLA desktop shelf and its hardware-fit trial. Never dispatches.

Run with the modelling Python environment (numpy/scipy). All dimensions mm.
The slot envelope is provisional until the owner's end-loaded nuts are fitted.
"""
import json
from collections import Counter
from pathlib import Path
import warnings
import numpy as np
from scipy.spatial.transform import Rotation

HERE = Path(__file__).resolve().parent
PARTS = HERE / 'parts'
SLOT_WIDTH = 10.0
SLOT_DEPTH = 3.0
MOUTH = 6.4


def rotation(x=0, y=0, z=0):
    return Rotation.from_euler('xyz', [x, y, z], degrees=True).as_matrix()


def numbers(values):
    return ' '.join(f'{float(x):.8g}' for x in values)


def main():
    PARTS.mkdir(exist_ok=True)
    sources, instances, bolts = {}, [], []

    def part(name, source, holes=(), down='bot'):
        (PARTS / (name + '.coscad')).write_text(source + '\n')
        sources[name] = {'holes_mm': holes, 'down': down}

    for length in (50, 160, 170, 230):
        rail_source = f'''// Custom printed 2020; end-loaded M5 nuts. NOT aluminium-rated.
// Slot: mouth {MOUTH}; nut pocket {SLOT_WIDTH} x {SLOT_DEPTH}, 1.5 mm lips.
// Narrow screw-tip relief reaches 6.5 mm below each face (M5x12 through 6).
// Four cavities leave diagonal webs; verify one connected solid after edits.
blank = box {length} 20 20
mouth = box {length+2} {MOUTH} 3.1 |> z 9.95
pocket = box {length+2} {SLOT_WIDTH} {SLOT_DEPTH} |> z 7
tip = box {length+2} 5.6 5 |> z 6
slot = mouth ⊕ pocket ⊕ tip
profile = blank ⊖ slot ⊖ (slot |> rotx 90) ⊖ (slot |> rotx 180) ⊖ (slot |> rotx 270) ⊖ (xcyl 2.1 {length+2})
// Print flat along X for beam strength and shorter toolpaths.
// Bed-only supports under the downward slot require cleanup and a nut-fit trial.
'''
        part(f'rail_{length}', rail_source + 'main = profile')
        part(f'rail_print_{length}', rail_source + 'main = profile')

    lholes = [[25, 0, 0], [45, 0, 0], [0, -25, 0], [0, -45, 0]]
    tholes = lholes + [[-25, 0, 0], [-45, 0, 0]]
    connector = '''// Flat gusset, 6 mm, two M5 screws per arm. Hole diameter 5.6 mm.
// Joint-centre relief avoids the perpendicular rail at six-leg intersections.
arm = box 45 20 6 |> x 32.5
stem = box 20 45 6 |> y -32.5
web = (box 1 1 6 |> move 10 -10 0) ⇓ (box 1 1 6 |> move 42 -10 0) ⇓ (box 1 1 6 |> move 10 -42 0)
right = arm ⊕ stem ⊕ web
relief = box 20.4 20.4 10
hole = zcyl 2.8 10
'''
    for name, holes, shape in [('gusset_l', lholes, 'right'), ('gusset_t', tholes, 'right ⊕ (right |> mirror 1 0 0)')]:
        cuts = ''.join(f' |> cut (hole |> move {numbers(h)})' for h in holes)
        part(name, connector + f'main = ({shape}) ⊖ relief' + cuts, holes)

    pholes = [[x, y, 0] for x in (-50, 50) for y in (-90, 90)]
    for side in ('left', 'right'):
        posts = (-90, 100) if side == 'left' else (-100, 90)
        ribs = [f'(box 20 200 8.1 |> move {x} 0 4.05)' for x in (-90, 90)]
        ribs += [f'(box 200 20 8.1 |> move 0 {y} 4.05)' for y in (-90, 90)]
        ribs += [f'(box 4 200 8.1 |> move {x} 0 4.05)' for x in (-50, 0, 50)]
        ribs += [f'(box 200 4 8.1 |> move 0 {y} 4.05)' for y in (-50, 0, 50)]
        # Model bottom at 0; top at 11. Print the broad top face against the bed.
        source = '// 200 mm panel, 3 mm skin over 8 mm ribs. Four recessed M5x12 mounts.\n'
        source += 'skin = box 200 200 3 |> z 9.5\nribs = ' + ' ⊕ '.join(ribs) + '\n'
        source += 'main = (skin ⊕ ribs) ∩ (box 199.6 199.6 12 |> z 5.5)'
        for x in posts:
            for y in (-90, 90):
                source += f' |> cut (box 20.6 20.6 30 |> move {x} {y} 5)'
        for x, y, _ in pholes:
            source += f' |> cut (zcyl 2.8 30 |> move {x} {y} 5)'
            source += f' |> cut (zcyl 5.5 10 |> move {x} {y} 11)'
        part('panel_' + side, source, pholes, 'top')

    part('panel_mount_coupon', '''// Same 11 mm rib depth / 6 mm remaining bearing as the full panel.
main = box 24 24 11 |> cut (zcyl 2.8 20) |> cut (zcyl 5.5 10 |> z 5.5)''', down='top')

    def place(name, label, xyz, r=None, kind='rail'):
        r = np.eye(3) if r is None else r
        with warnings.catch_warnings():
            warnings.simplefilter('ignore', UserWarning)
            angles = Rotation.from_matrix(r).as_euler('xyz', degrees=True)
        entry = dict(id=label, part=name, kind=kind, position_mm=list(xyz),
                     rotation_xyz_deg=np.round(angles, 8).tolist())
        instances.append(entry)
        return entry

    def hardware(owner, host, face, at, point):
        bolts.append(dict(part=owner['id'], host=host['id'], face=face, at_mm=at,
                          hole_world_mm=np.round(point, 8).tolist(), bolt='M5x12',
                          nut='M5 sliding T-nut for 6 mm-slot 2020', through_mm=6))

    posts, xr, yr = {}, {}, {}
    for x in (-190, 0, 190):
        for y in (-90, 90):
            posts[x, y] = place('rail_230', f'post_{x+190}_{y+90}', [x, y, 115], rotation(y=-90), 'post')
    for tier, z in enumerate((60, 220), 1):
        for x in (-95, 95):
            for y in (-90, 90):
                xr[tier, x, y] = place('rail_170', f'xrail_{tier}_{x+95}_{y+90}', [x, y, z], rotation())
        for x in (-190, 0, 190):
            yr[tier, x] = place('rail_160', f'yrail_{tier}_{x+190}', [x, 0, z], rotation(z=90))

        for x in (-190, 0, 190):
            for y in (-90, 90):
                # Plates outside the front/back frame. T plates share the centre post.
                kind = 'gusset_t' if x == 0 else 'gusset_l'
                sign = -1 if x == 190 else 1
                r = rotation(x=90) @ rotation(y=180 if sign == -1 else 0)
                origin = np.array([x, y + (13 if y > 0 else -13), z])
                plate = place(kind, f'xgusset_{tier}_{x+190}_{y+90}', origin.tolist(), r, 'connector')
                for h in sources[kind]['holes_mm']:
                    p = origin + r @ h
                    host = posts[x, y] if h[0] == 0 else xr[tier, -95 if p[0] < 0 else 95, y]
                    start = 0 if h[0] == 0 else host['position_mm'][0] - 85
                    hardware(plate, host, 'bak' if y > 0 else 'fwd', p[2] if h[0] == 0 else p[0] - start, p)

                # Y plates: centre ones sit inside the bay; relieved centre clears X rails.
                r = rotation(z=90) @ rotation(x=90) @ rotation(y=180 if y > 0 else 0)
                origin = np.array([x + (-13 if x < 0 else 13), y, z])
                plate = place('gusset_l', f'ygusset_{tier}_{x+190}_{y+90}', origin.tolist(), r, 'connector')
                for h in lholes:
                    p = origin + r @ h
                    host = posts[x, y] if h[0] == 0 else yr[tier, x]
                    hardware(plate, host, 'lft' if x < 0 else 'rt', p[2] if h[0] == 0 else p[1] + 80, p)

        for side, x in [('left', -100), ('right', 100)]:
            panel = place('panel_' + side, f'panel_{tier}_{side}', [x, 0, z+10], kind='panel')
            for hx, hy, _ in pholes:
                px = x + hx
                host = xr[tier, -95 if px < 0 else 95, hy]
                hardware(panel, host, 'top', px - (host['position_mm'][0]-85), [px, hy, z+16])

    lines = ['// Generated by build_model.py. Design target: 1 kg uniformly distributed per tier.',
             '// Sliding nuts MUST be preloaded before rail ends are closed.', 'plate 256 256 6 250', 'clearance 0']
    for i in instances:
        meta = sources[i['part']]
        lines.append(f"{i['id']} ← parts/{i['part']}.coscad ×1 ▽{meta['down']} material=pla brim=5" + (' profile=2020 preload=hand' if i['kind'] in ('rail', 'post') else ''))
    lines += [''] + [f"fixed {i['id']} {numbers(i['position_mm'])} {numbers(i['rotation_xyz_deg'])}" for i in instances]
    lines += [''] + [f"fastener M5x12 {b['part']} {b['host']} {b['face']} at={b['at_mm']:.8g} through=6 nut=slidein head=button" for b in bolts]
    (HERE / 'shelf.assemble').write_text('\n'.join(lines) + '\n')
    (HERE / 'fit.assemble').write_text('''// Assemble the coupon by hand only after printing. No machine dispatch.
plate 256 256 6 250
rail ← parts/rail_print_50.coscad ×1 ▽bot material=pla brim=5
gusset ← parts/gusset_l.coscad ×1 ▽bot material=pla brim=5
panel ← parts/panel_mount_coupon.coscad ×1 ▽top material=pla brim=5
asm = rail ⊕ (gusset |> x 90) ⊕ (panel |> x -65)
''')
    counts = Counter(i['part'] for i in instances)
    manufacturing = ['// Manufacturing inventory; assembled geometry is shelf.assemble.', 'plate 256 256 6 250']
    for name, count in counts.items():
        source = name.replace('rail_', 'rail_print_') if name.startswith('rail_') else name
        manufacturing.append(f"{name} ← parts/{source}.coscad ×{count} ▽{sources[source]['down']} material=pla brim=5")
    manufacturing.append('asm = ' + ' ⊕ '.join(f'({name} |> x {idx*300})' for idx, name in enumerate(name for name, count in counts.items() for _ in range(count))))
    (HERE / 'manufacture.assemble').write_text('\n'.join(manufacturing)+'\n')
    data = dict(schema='coscad.shelf-study/1', load_target_kg_per_tier=1,
                deck_mm=[400, 200], top_heights_mm=[81, 241], clear_between_tiers_mm=129,
                slot=dict(mouth_mm=MOUTH, pocket_width_mm=SLOT_WIDTH, pocket_depth_mm=SLOT_DEPTH,
                          lip_mm=1.5, screw_tip_depth_mm=6.5, physical_fit_verified=False),
                parts=sources, instances=instances, hardware=bolts,
                bom=dict(printed=dict(Counter(i['part'] for i in instances)), M5x12=len(bolts), sliding_M5_nuts=len(bolts)),
                physical_load_tested=False, dispatched=False)
    (HERE / 'model.json').write_text(json.dumps(data, indent=2) + '\n')
    (HERE / 'process.json').write_text(json.dumps(dict(wall_loops='6', sparse_infill_density='35%',
        sparse_infill_pattern='gyroid', top_shell_layers='5', bottom_shell_layers='5',
        enable_support='1', support_type='normal(auto)', support_on_build_plate_only='1', layer_height='0.2', outer_wall_speed='50', inner_wall_speed='100', default_acceleration='3000', outer_wall_acceleration='1000', inner_wall_acceleration='3000', travel_acceleration='5000'), indent=2)+'\n')
    print(json.dumps(data['bom'], indent=2))


if __name__ == '__main__':
    main()
