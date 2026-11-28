# Send this job to the X1C once (Sol port of dispatch.py, 2026-11-28). SG90 press-fit revision 2 in PLA from the external spool.
# The spools are matched from the printer's live report; models/lib/bambu.sol does the rest.
# print-run.json records the original dispatch, so this refuses to send again (--dry-run still checks).
#
#   sol dispatch.sol --dry-run
#   sol dispatch.sol --confirm-bed-clear --note "owner confirmed ..."
Bambu = use "../../../lib/bambu".
> dir = "{Bambu.repoRoot Unit}/models/sg90-rack/press-fit/x1c-r2";
  Bambu.main {dir = dir, printer = "x1c", package = "{dir}/slice/sg90-press-fit-r2.gcode.3mf", remote = "sg90-press-fit-r2-x1c-{Bambu.stamp Unit}.gcode.3mf",
              filaments = [{key = "pla", material = "PLA", colour = "", idx = "", preset = ""}], bed = "auto", options = []}.
