# Send one build.sol output folder of the AGVN sailboat to the X2D, once (Sol port of
# dispatch_x2d.py, 2026-11-28). The spools come from ../parts.json: each tagged filament is matched
# to a loaded spool in the printer's live report, and the start is refused if the package was
# sliced for a different nozzle map. Everything else is models/lib/bambu.sol `dispatch`.
#
#   sol dispatch.sol --job tree-two-nozzles-notower-cool10 --dry-run
#   sol dispatch.sol --job tree-two-nozzles-notower-cool10 --confirm-bed-clear --note "owner confirmed ..."
Bambu = use "../../../../lib/bambu".
> argv = args Unit;
  name = Bambu.argAfter "--job" argv;
  root = Bambu.repoRoot Unit;
  sail = "{root}/models/twin-boat-trials/agvn/sail";
  dir = "{sail}/x2d-light/{name}";
  remote = if name == "tree-two-nozzles" then "AGVN-sail-light-2n.gcode.3mf" else "AGVN-{name}.gcode.3mf";
  if name == "" then error "usage: sol dispatch.sol --job NAME [--dry-run | --confirm-bed-clear --note TEXT]" else
  Bambu.main {dir = dir, printer = "x2d", package = "{dir}/slice/AGVN-sail-light.gcode.3mf", remote = remote,
              filaments = Bambu.filamentsOf (Bambu.readTags sail), bed = "textured_plate", options = []}.
