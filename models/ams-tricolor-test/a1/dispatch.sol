# Send this job to the A1 once (Sol port of dispatch.py, 2026-11-28). AMS tray 1 white, tray 2 black, tray 3 yellow PLA (the three-colour AMS Lite test).
# The spools are matched from the printer's live report; models/lib/bambu.sol does the rest.
# print-run.json records the original dispatch, so this refuses to send again (--dry-run still checks).
#
#   sol dispatch.sol --dry-run
#   sol dispatch.sol --confirm-bed-clear --note "owner confirmed ..."
Bambu = use "../../lib/bambu".
> dir = "{Bambu.repoRoot Unit}/models/ams-tricolor-test/a1";
  Bambu.main {dir = dir, printer = "a1", package = "{dir}/slice/ams-tricolor-a1.gcode.3mf", remote = "ams-tricolor-a1-{Bambu.stamp Unit}.gcode.3mf",
              filaments = [{key = "white", material = "PLA", colour = "#FFFFFF", idx = "", preset = ""}, {key = "black", material = "PLA", colour = "#000000", idx = "", preset = ""}, {key = "yellow", material = "PLA", colour = "#F4EE2A", idx = "", preset = ""}], bed = "auto", options = [("flow_cali", Bambu.J.JBool False)]}.
