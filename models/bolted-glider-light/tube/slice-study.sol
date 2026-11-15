# Slice this variant's nose and fuselage with the thin-wall process (wings from
# ../shell/wing-study-slices.json) and integrate object extrusion; with
# GLIDER_FEA_DIR set, also export the fuselage G-code and offsets.
# Usage: "$SOL" models/bolted-glider-light/tube/slice-study.sol [--slicer PATH]
S = use "../slice-study-common".
# Screw-on nose hardware.
main : unsafe Unit -> Unit.
main Unit = S.main "models/bolted-glider-light/tube" "tube" ["nose","fuselage"] True [(5, 0 - 53)].
> main Unit.
