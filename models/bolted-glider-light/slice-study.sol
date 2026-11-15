# Slice centred, axis-aligned individual parts and integrate object extrusion.
# Usage: "$SOL" models/bolted-glider-light/slice-study.sol [--slicer PATH]
S = use "slice-study-common".
# Two covered faces, 10% allowance for curvature, seams and adhesive; then the nose hardware.
main : unsafe Unit -> Unit.
main Unit = S.main "models/bolted-glider-light" "" S.allParts False [(2 * 0.04442 * 1.10 * 25, 115), (5, 0 - 53)].
> main Unit.
