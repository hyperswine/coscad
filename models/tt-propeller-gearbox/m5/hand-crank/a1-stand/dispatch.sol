# Send this job to the A1 once (Sol port of dispatch.py, 2026-11-28). The hand-crank stand in black PLA from the AMS Lite.
# The spools are matched from the printer's live report; models/lib/bambu.sol does the rest.
# print-run.json records the original dispatch, so this refuses to send again (--dry-run still checks).
#
#   sol dispatch.sol --dry-run
#   sol dispatch.sol --confirm-bed-clear --note "owner confirmed ..."
Bambu = use "../../../../lib/bambu".
> dir = "{Bambu.repoRoot Unit}/models/tt-propeller-gearbox/m5/hand-crank/a1-stand";
  Bambu.main {dir = dir, printer = "a1", package = "{dir}/slice/hand-crank-stand-a1.gcode.3mf", remote = "hand-crank-stand-a1-{Bambu.stamp Unit}.gcode.3mf",
              filaments = [{key = "black", material = "PLA", colour = "#000000", idx = "", preset = ""}], bed = "auto", options = [("flow_cali", Bambu.J.JBool False)]}.
