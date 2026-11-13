# Generate hulls, hydrostatic-envelope views and the saved process intent.
C = use "sol/lib/cli".
J = use "sol/lib/json".
P = use "sol/lib/proc".
# Stations are in hundredths of a millimeter. The outline is in percent
# of length/width. Integer products keep the decimal CoScad source stable.
monoOuter = [(0,9200,1800),(300,9600,2200),(1500,10800,4600),(2800,11600,5400)].
monoInner = [(600,8700,1600),(1600,10100,3900),(2830,11000,4800)].
centerOuter = [(0,8000,2000),(1000,9400,3400),(2000,10000,3800)].
centerInner = [(240,7500,1400),(800,8700,2600),(2030,9600,3400)].
pontoonOuter = [(0,9000,800),(800,10300,2000),(1200,11000,2200),(2200,9200,200)].
pontoonInner = [(240,8500,400),(800,9800,1600),(1100,10500,1800),(2000,9000,20)].
outline = [(-50,-30),(-50,-46),(-45,-50),(-35,-50),(-20,-50),(0,-50),(15,-50),(35,-48),
           (48,-18),(50,0),(48,18),(35,48),(15,50),(0,50),(-20,50),(-35,50),(-45,50),
           (-50,46),(-50,30),(-50,10),(-50,-10),(-50,-30)].
fraction n =
  places = if n % 1000 == 0 then [1000] else if n % 100 == 0 then [1000,100]
           else if n % 10 == 0 then [1000,100,10] else [1000,100,10,1];
  Str.join "" (List.map (fn place -> str ((n / place) % 10)) places).
formatMm a b =
  signed = a * (10000 / b);
  units = if signed < 0 then 0 - signed else signed;
  sign = if signed < 0 then "-" else "";
  remainder = units % 10000;
  sign + str (units / 10000) + (if remainder == 0 then "" else "." + fraction remainder).
section row =
  (height,length,width) = row;
  point = fn p -> (x,y) = p; formatMm (x * length) 10000 + " " + formatMm (y * width) 10000;
  "(ζ " + formatMm height 100 + " (⮕ 0.02 (✎ " + Str.join " " (List.map point outline) + ")))".
hull rows = Str.join " ⇓ " (List.map section rows).
box x y z = "χ {x - 3} (ψ " + formatMm (2 * y - 3) 2 + " (ζ {z} (▬ 6 3 3)))".
arm x side =
  a = box x (16 * side) 9;
  b = box x (24 * side) 15;
  c = box x (31 * side) 9;
  ["({a} ⇓ {b})","({b} ⇓ {c})"].
views =
  outer = hull monoOuter;
  inner = hull monoInner;
  center = "center_outer = " + hull centerOuter + "\ncenter_void = " + hull centerInner + "\ncenter_hull = center_outer ⊖ center_void";
  pontoon = "pontoon_outer = " + hull pontoonOuter + "\npontoon_void = " + hull pontoonInner + "\npontoon = pontoon_outer ⊖ pontoon_void";
  arms = arm (-24) (-1) + arm (-24) 1 + arm 24 (-1) + arm 24 1;
  envelope = hull centerOuter + " ⊕ (ψ -38 (" + hull pontoonOuter + ")) ⊕ (ψ 38 (" + hull pontoonOuter + "))";
  [("monohull","outer = {outer}\nvoid = {inner}","outer ⊖ void"),
   ("monohull-envelope","",outer),("monohull-opening","",hull [(2800,11000,4800),(2802,11000,4800)]),
   ("outrigger",center + "\n" + pontoon,"center_hull ⊕ (ψ -38 pontoon) ⊕ (ψ 38 pontoon) ⊕ " + Str.join " ⊕ " arms),
   ("outrigger-envelope","",envelope),("outrigger-opening","",hull [(2000,9400,3200),(2002,9400,3200)])].
stations rows = J.JArr (List.map (fn row -> (z,l,w) = row; J.nums (List.map (fn n -> Numeric.div n 100) [z,l,w])) rows).
design =
  mono = J.JObj [("printer",J.JStr "P1S"),("outer_stations",stations monoOuter),("inner_stations",stations monoInner),
    ("aim",J.JStr "Open hull with thick low keel and flared sides, restoring buoyancy at moderate heel, not guaranteed recovery from full capsize")];
  out = J.JObj [("printer",J.JStr "A1"),("center_stations",stations centerOuter),("pontoon_stations",stations pontoonOuter),
    ("pontoon_centerlines_y",J.nums [-38,38]),("aim",J.JStr "Low, wide central hull with two side outriggers and arched support-free connecting arms")];
  process = J.JObj [("material",J.JStr "PLA"),("nozzle_mm",J.JNum 0.4),("layer_mm",J.JNum 0.16),("wall_loops",J.JNum 6),
    ("infill",J.JStr "100%"),("supports",J.JBool False),
    ("reason",J.JStr "Solid low-keel geometry and multiple continuous wall paths; open hulls avoid internal supports. Water tightness still requires a physical leak test.")];
  J.pretty (J.JObj [("units",J.JStr "mm"),("conventional",mono),("outrigger",out),("process_intent",process)]) + "\n".
compileView tmp compiler entry =
  (name,defs,expression) = entry;
  source = "// Small floating boat, mm. Open cavities require no trapped support.\n{defs}\nmain = {expression}\n";
  path = "{tmp}/{name}.coscad";
  stage = PipelineSpec [P.spec ["cat"]] (PipeBytes (unwrap (Utf8.encode source))) (PipeFileNew path) PipeOutputInherit 0 PipeFail;
  _ = case Proc.pipeline stage of Ok (PipelineResult 0 _ _ _) -> Unit | other -> error "cannot stage {name}: {other}";
  _ = unwrap (P.output (Proc.query (P.spec [compiler,path,"-o","{tmp}/{name}.scad"])));
  (name,source,readPath "{tmp}/{name}.scad").
publish dir row =
  (name,source,scad) = row;
  _ = writePath "{dir}/{name}.coscad" source;
  writePath "{dir}/{name}.scad" scad.
> usage = "usage: design.sol [--coscad PATH] [--out DIR]";
  argv = args Unit;
  if List.has "--help" argv or List.has "-h" argv then print usage else
    opts = unwrap (C.parse [C.Option "coscad" "" C.One,C.Option "out" "" C.One] argv);
    _ = if C.operands opts == [] then Unit else error usage;
    base = if exists "models/twin-boat-trials/design.sol" then "models/twin-boat-trials" else ".";
    dir = C.valueOr base "out" opts;
    compiler = C.valueOr "" "coscad" opts;
    exe = if compiler != "" then compiler else Str.trim (unwrap (P.output (Proc.query (P.inDir "{base}/../.." (P.spec ["cabal","list-bin","exe:coscad"])))));
    _ = unwrap (P.output (Proc.query (P.spec ["mkdir","-p",dir])));
    tmp = tempDir "coscad-twin-boats";
    compiled = List.map (compileView tmp exe) views;
    _ = List.map (publish dir) compiled;
    writePath "{dir}/design.json" design.
