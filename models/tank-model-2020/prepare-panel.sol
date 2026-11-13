# Generate the single- and double-flange tank rail coupons from the wall profile.
C = use "sol/lib/cli".
P = use "sol/lib/proc".
command argv = case Proc.streamNow (P.spec argv) of
    Ok 0 -> Unit | Ok code -> error "{argv} exited {code}" | Err why -> error why.
binding name value source =
  prefix = "{name} = ";
  # A final sentinel retains trailing empty lines across the split/join.
  lines = Str.split 10 (source + "\n.");
  matching = List.filter (Str.startsWith prefix) lines;
  if List.len matching != 1 then error "expected exactly one {name} binding" else
    rendered = Str.join "\n" (List.map (fn line -> if Str.startsWith prefix line then prefix + value else line) lines);
    Str.slice rendered 0 (Str.len rendered - 2).
oneSide = "\n// Crop the actual keyed edge; flat print face is at Z=0.\n" +
          "main = panel |> isect (box 9 30 16 |> move 31.5 0 0) |> move -31.5 0 (neck / 2)\n".
twoSide = Str.join "\n" ["","// Complete stepped T-head: matching flanges on BOTH faces of the neck.",
  "// 2 mm plain noses, then 3 mm ramps to the full profile at either end.",
  "bilateral = panel |> add (⇋ 0 0 1 panel)",
  "coupon = bilateral |> isect (box 9 30 16 |> move 31.5 0 0) |> x -31.5",
  "upright = coupon |> rotx 90 |> z 15",
  "// Removable first-layer ears stay 0.5 mm outside the rail face (X=-1.5).",
  "ears = (zcyl 3 0.2 |> move -5 (neck / 2) 0.1)",
  "  |> add (zcyl 3 0.2 |> move -5 (-neck / 2) 0.1)","main = upright |> add ears",""].
coupon bilateral wall holes clearance code =
  panel = wall |> binding "clearance" clearance |> Str.replace "main =" "panel =";
  shape = if bilateral then
    "// V3: the one-sided panel below is only an intermediate.\n" +
    "// The final sample reflects its flanges, crops one edge and prints upright.\n" +
    (panel |> binding "key_length" "26" |> binding "lead" "3") + twoSide
    else panel + oneSide;
  cuts = List.map (fn i -> offset = i * 4 - (holes - 1) * 2;
    if bilateral then "  |> cut (box 1 16 1 |> roty 45 |> move -3 0 {15 + offset})\n"
    else "  |> cut (zcyl 0.7 16 |> move -3.5 {offset} 4)\n") (List.range 0 (holes - 1));
  stem = if bilateral then "double" else "panel";
  ("{stem}-c{code}-{holes}holes",shape + Str.join "" cuts).
# Put sources on disk for external compilers before they run. Render the whole
# set in private so a bad source cannot replace an earlier verified mesh.
render compiler stage row =
  (name,source) = row;
  path = "{stage}/{name}.coscad";
  pipe = PipelineSpec [P.spec ["cat"]] (PipeBytes (unwrap (Utf8.encode source))) (PipeFileNew path) PipeOutputInherit 0 PipeFail;
  _ = case Proc.pipeline pipe of Ok (PipelineResult 0 _ _ _) -> Unit | other -> error "cannot stage {path}: {other}";
  _ = command [compiler,"stl",path,"-o","{stage}/{name}.stl"];
  command ["test","-s","{stage}/{name}.stl"].
prepare variant opts =
  bilateral = variant == "v3";
  subdir = if bilateral then "panel-fit-v3-double-flange" else "panel-fit-v2";
  base = if exists "models/tank-model-2020/prepare-panel.sol" then "models/tank-model-2020"
         else if exists "prepare-panel.sol" then "." else "..";
  readSource name = readPath "{base}/{name}" |> Str.replace "\r\n" "\n" |> Str.replace "\r" "\n";
  wall = readSource "panel_wall.coscad";
  rail = readSource "rail100.coscad" |> binding "len" "35" |> Str.replace "main =" "profile =";
  sources = [coupon bilateral wall 1 "0.2" "020",coupon bilateral wall 2 "0.25" "025",coupon bilateral wall 3 "0.3" "030",
             ("rail35",rail + "\nmain = profile |> z (len / 2)\n")];
  dest = C.valueOr "{base}/{subdir}" "output" opts;
  _ = command ["mkdir","-p",dest];
  _ = if C.flag "render" opts then
    stage = tempDir "coscad-panel-fit";
    _ = List.map (render (C.valueOr "coscad" "coscad" opts) stage) sources;
    artifacts = ls stage |> List.filter (fn path -> Str.endsWith ".scad" path or Str.endsWith ".stl" path);
    _ = List.map (fn name -> case Fs.copyPublishNow "{stage}/{name}" "{dest}/{name}" 65536 True of
                       Ok size -> command ["chmod","=rw","{dest}/{name}"] | Err why -> error (str why)) artifacts;
    Unit
    else Unit;
  _ = List.map (fn row -> (name,source) = row; writePath "{dest}/{name}.coscad" source) sources;
  Unit.
> opts = unwrap (C.parse [C.Option "output" "" C.One,C.Option "coscad" "" C.One,C.Option "render" "" C.Switch] (args Unit));
  case C.operands opts of
    [variant] -> if List.has variant ["v2","v3"] then prepare variant opts else error "expected v2 or v3"
  | _ -> error "usage: prepare-panel.sol v2|v3 [--output DIR] [--render] [--coscad PATH]".
