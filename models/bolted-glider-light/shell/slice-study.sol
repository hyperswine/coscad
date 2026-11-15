# Slice the shell variant's parts with the thin-wall process and integrate object
# extrusion; with GLIDER_FEA_DIR set, also export the fuselage G-code and offsets.
# Usage: "$SOL" models/bolted-glider-light/shell/slice-study.sol [--slicer PATH]
S = use "../slice-study-common".
# Screw-on nose hardware.
main : unsafe Unit -> Unit.
main Unit = S.main "models/bolted-glider-light/shell" "shell" S.allParts False [(5, 0 - 53)].
> main Unit.
