"""Read saddle-frame gauges from a CoScad wing-saddle solve (mm, radians)."""
import argparse
import json
import math
from pathlib import Path


def read_gauges(report):
    result = report["result"]
    def positions(name):
        body = next(body for body in result["bodies"] if body["name"] == name)
        return {tuple(round(x, 7) for x in row["position_mm"]): row["displacement_mm"]
                for row in body["nodes"]}

    nodes = positions("wing")

    # Track the angle of a marked spanwise baseline on the wing underside.
    near, far = (8, 40, 0), (24, 40, 0)
    def angle(points):
        a, b = points[near], points[far]
        return math.atan2(b[2] - a[2], far[0] - near[0] + b[0] - a[0])

    rotation = angle(nodes) - angle(positions("body"))
    # Centre of the underside load pad (Hex20 has edge midsides, no face centre).
    z = sum(nodes[x, y, 0][2] for x in (184, 188) for y in (172, 176)) / 4
    return dict(load_pad_corner_mean_z_mm=z, root_baseline_angle_rad=rotation,
                baseline_mm=[near, far], degrees_of_freedom=result["degrees_of_freedom"],
                iterations=result["iterations"], relative_residual=result["free_residual_relative"],
                equilibrium=result["equilibrium_residual"],
                interface_wrenches={i["name"]: i["on_a"] for i in result["interfaces"]})


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("report", type=Path)
    args = parser.parse_args()
    print(json.dumps(read_gauges(json.loads(args.report.read_text())), indent=2))
