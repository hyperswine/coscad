"""Draw the sampled XY footprint and selected physical patches from --preview."""
import argparse
from html import escape
import json
from pathlib import Path

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("report", type=Path)
parser.add_argument("svg", type=Path)
args = parser.parse_args()
if args.svg.resolve() == args.report.resolve():
    parser.error("choose a separate output file")
preview = json.loads(args.report.read_text())["preview"]
geometry = preview["geometry"]
bounds = [(x + dx, y + dy) for body in geometry for x, y in body["xy_footprint_mm"]
          for dx, dy in ((0, 0), (body["xy_cell_mm"], body["xy_cell_mm"]))]
left, bottom = map(min, zip(*bounds))
right, top = map(max, zip(*bounds))
svg = [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="{left-2} {-top-2} {right-left+4} {top-bottom+4}">',
       '<title>Assembly regions in the analysis XY plane</title>']
colors = ("#9bc8d2", "#e4ba94", "#b6c6a8")
for index, body in enumerate(geometry):
    h = body["xy_cell_mm"]
    path = " ".join(f"M{x},{-y}h{h}v{-h}h{-h}z" for x, y in body["xy_footprint_mm"])
    svg.append(f'<path d="{path}" fill="{colors[index % len(colors)]}" opacity=".65">'
               f'<title>{escape(body["instance"])}</title></path>')
for region in preview["regions"]:
    role = region["role"]
    color = {"support": "#225baf", "load": "#bd3027"}.get(role, "#188148")
    for square in region["faces_mm"]:
        points = " ".join(f"{x},{-y}" for x, y, _ in square)
        svg.append(f'<polygon points="{points}" fill="{color}" stroke="{color}" stroke-width=".5">'
                   f'<title>{escape(role)} on {escape(region["instance"])}</title></polygon>')
args.svg.write_text("\n".join(svg + ["</svg>"]) + "\n")
