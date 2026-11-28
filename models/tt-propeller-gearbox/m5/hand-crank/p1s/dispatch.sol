# Send this job to the P1S once (Sol port of dispatch.py, 2026-11-28). The hand crank in PLA from the external spool.
# The spools are matched from the printer's live report; models/lib/bambu.sol does the rest.
# print-run.json records the original dispatch, so this refuses to send again (--dry-run still checks).
#
#   sol dispatch.sol --dry-run
#   sol dispatch.sol --confirm-bed-clear --note "owner confirmed ..."
Bambu = use "../../../../lib/bambu".
> dir = "{Bambu.repoRoot Unit}/models/tt-propeller-gearbox/m5/hand-crank/p1s";
  Bambu.main {dir = dir, printer = "p1s", package = "{dir}/slice/hand-crank-p1s.gcode.3mf", remote = "hand-crank-{Bambu.stamp Unit}.gcode.3mf",
              filaments = [{key = "pla", material = "PLA", colour = "", idx = "", preset = ""}], bed = "auto", options = []}.
