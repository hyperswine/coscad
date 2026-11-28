# Send the long tweezers slice for one printer, once (2026-11-28). The tagged PLA (optionally a
# colour) is matched to a loaded spool from the printer's live report; models/lib/bambu.sol does
# the checks, upload, start and first-layer watch.
#
#   sol dispatch.sol --printer x2d --colour "#000000" --dry-run
#   sol dispatch.sol --printer x2d --colour "#000000" --confirm-bed-clear --note "owner confirmed ..."
Bambu = use "../lib/bambu".
> argv = args Unit;
  printer = Bambu.argAfter "--printer" argv;
  colour = Bambu.argAfter "--colour" argv;
  dir = "{Bambu.repoRoot Unit}/models/long-tweezers";
  job = "{dir}/{printer}";
  bed = if printer == "x2d" then "textured_plate" else "auto";
  if printer == "" then error "usage: sol dispatch.sol --printer P [--colour #RRGGBB] [--dry-run | --confirm-bed-clear --note TEXT]" else
  Bambu.main {dir = job, printer = printer, package = "{job}/slice/long-tweezers-{printer}.gcode.3mf",
              remote = "long-tweezers-{printer}-{Bambu.stamp Unit}.gcode.3mf",
              filaments = [{key = "pla", material = "PLA", colour = colour, idx = "", preset = ""}], bed = bed, options = []}.
