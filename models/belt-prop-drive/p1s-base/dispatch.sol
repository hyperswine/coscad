# Send this job to the P1S once (Sol port of dispatch.py, 2026-11-28). The belt drive base in PLA from the external spool.
# The spools are matched from the printer's live report; models/lib/bambu.sol does the rest.
# print-run.json records the original dispatch, so this refuses to send again (--dry-run still checks).
#
#   sol dispatch.sol --dry-run
#   sol dispatch.sol --confirm-bed-clear --note "owner confirmed ..."
Bambu = use "../../lib/bambu".
> dir = "{Bambu.repoRoot Unit}/models/belt-prop-drive/p1s-base";
  Bambu.main {dir = dir, printer = "p1s", package = "{dir}/slice/belt-p1s-base.gcode.3mf", remote = "belt-base-{Bambu.stamp Unit}.gcode.3mf",
              filaments = [{key = "pla", material = "PLA", colour = "", idx = "", preset = ""}], bed = "auto", options = []}.
