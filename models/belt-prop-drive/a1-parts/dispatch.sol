# Send this job to the A1 once (Sol port of dispatch.py, 2026-11-28). The belt propeller drive parts in white PLA from the AMS Lite.
# The spools are matched from the printer's live report; models/lib/bambu.sol does the rest.
# print-run.json records the original dispatch, so this refuses to send again (--dry-run still checks).
#
#   sol dispatch.sol --dry-run
#   sol dispatch.sol --confirm-bed-clear --note "owner confirmed ..."
Bambu = use "../../lib/bambu".
> dir = "{Bambu.repoRoot Unit}/models/belt-prop-drive/a1-parts";
  Bambu.main {dir = dir, printer = "a1", package = "{dir}/slice/belt-a1-parts.gcode.3mf", remote = "belt-parts-a1-{Bambu.stamp Unit}.gcode.3mf",
              filaments = [{key = "white", material = "PLA", colour = "#FFFFFF", idx = "", preset = ""}], bed = "auto", options = [("flow_cali", Bambu.J.JBool False)]}.
