# Shared driver for the light-glider slice studies (this folder, shell, tube and
# tube-reinforced). Each part is centred and turned axis-aligned, swapped into
# the balanced glider's X1C fuselage project and sliced with Bambu Studio; the
# object extrusion is then integrated into a mass and a fore-aft centroid.
J = use "sol/lib/json".
P = use "sol/lib/proc".
C = use "sol/lib/cli".
N = use "../lib/num".
R = use "../lib/root".
Map = use "std/map".
O = use "std/option".

defaultSlicer = "/Applications/BambuStudio.app/Contents/MacOS/BambuStudio".
allParts = ["wing-right","wing-left","nose","fuselage"].

# math.cos/math.sin(math.radians(a)) from the platform libm, written out so the
# vertex text and sums match the Python study bit for bit (num.sol's series can
# differ by 1 ulp at these angles); `turn` checks them against num.sol.
turnDegrees name = case name of
    "wing-right" -> 1.5 | "wing-left" -> 88.5 | "nose" -> 0 | "fuselage" -> 45
  | _ -> error "unknown part: {name}".
libmCosSin d =
  if d == 1.5 then (0.9996573249755573, 0.026176948307873153)
  else if d == 88.5 then (0.02617694830787314, 0.9996573249755573)
  else if d == 45 then (0.7071067811865476, 0.7071067811865475)
  else if d == 2.5 then (0.9990482215818578, 0.043619387365336)
  else if d == 0 then (1.0, 0.0)
  else error "no libm cos/sin recorded for {d} degrees".
close a b = Numeric.abs (a - b) <= 0.000000000000001.
turn d =
  (c, s) = libmCosSin d;
  a = d * Num.div N.pi 180;
  if and (close c (N.cos a)) (close s (N.sin a)) then (c, s) else error "no cos/sin for {d} degrees".

# ---- small helpers ----------------------------------------------------------
checked argv = case Proc.streamNow (P.spec argv) of
    Ok 0 -> Unit
  | Ok code -> error "exit {code}: {argv}"
  | Err why -> error (str why).
output argv = unwrap (P.output (Proc.query (P.spec argv))).
put path contents =
  pipe = PipelineSpec [P.spec ["cat"]] (PipeBytes (unwrap (Utf8.encode contents))) (PipePublish path True) PipeOutputInherit 0 PipeFail;
  case Proc.pipeline pipe of Ok (PipelineResult 0 _ _ _) -> Unit | other -> error "cannot write {path}: {other}".
parent path = Str.join "/" (List.take (List.len (Str.split 47 path) - 1) (Str.split 47 path)).
float x = x * 1.0.
# Python float() of -?[0-9.]+ and of STL tokens ("1.", ".5" and "-.5" included).
pyFloat s =
  (sign, body) = if Str.startsWith "-" s then ("-", N.drop 1 s) else ("", s);
  b1 = if Str.startsWith "." body then "0" + body else body;
  b2 = if Str.endsWith "." b1 then b1 + "0" else b1;
  case Try.parseNum (sign + b2) of
    Ok v -> if body == "." then error "could not convert string to float: {s}" else float v
  | Err e -> error "could not convert string to float: {s}".

# ---- Python json.dumps (ensure_ascii, ", " / ": " separators, indent) ---------
hex4 n = N.padLeft 4 "0" (Str.join "" (List.map (fn k -> Str.sub "0123456789abcdef" ((n / k) % 16) 1) [4096,256,16,1])).
pyCode c = case c of
    34 -> "\\\"" | 92 -> "\\\\" | 10 -> "\\n" | 13 -> "\\r" | 9 -> "\\t" | 8 -> "\\b" | 12 -> "\\f"
  | _ -> if and (c >= 32) (c <= 126) then Str.fromCode c
         else if c > 65535 then
           v = c - 65536;
           "\\u{hex4 (55296 + v / 1024)}\\u{hex4 (56320 + v % 1024)}"
         else "\\u{hex4 c}".
plain c = and (and (c >= 32) (c <= 126)) (and (c != 34) (c != 92)).
pyQuote t = if List.all plain (Str.codes t) then "\"{t}\"" else "\"{Str.join "" (List.map pyCode (Str.codes t))}\"".
pyScalar j = case j of
    J.JNull -> "null" | J.JBool True -> "true" | J.JBool False -> "false"
  | J.JNum n -> N.repr n | J.JStr t -> pyQuote t | _ -> error "not a scalar".
dumps : unsafe J.Json -> String.
dumps j = case j of
    J.JArr xs -> "[{Str.join ", " (List.map dumps xs)}]"
  | J.JObj kvs -> "\{{Str.join ", " (List.map (fn p -> (k, v) = p; "{pyQuote k}: {dumps v}") kvs)}\}"
  | _ -> pyScalar j.
dumpsIndent : unsafe String -> J.Json -> String.
dumpsIndent ind j = case j of
    J.JArr (x :: xs) -> "[\n{Str.join ",\n" (List.map (fn v -> "{ind}  {dumpsIndent "{ind}  " v}") (x :: xs))}\n{ind}]"
  | J.JObj (p :: ps) -> "\{\n{Str.join ",\n" (List.map (fn q -> (k, v) = q; "{ind}  {pyQuote k}: {dumpsIndent "{ind}  " v}") (p :: ps))}\n{ind}\}"
  | J.JArr [] -> "[]" | J.JObj [] -> "\{\}"
  | _ -> pyScalar j.
numAt key j = unwrap (J.get key j |>? J.num).

# ---- mesh -------------------------------------------------------------------
stlPoints path =
  rows = List.filter (fn line -> Str.startsWith "vertex " (Str.trim line)) (Str.lines (readPath path));
  List.map (fn line -> case List.map pyFloat (List.drop 1 (Str.words (Str.trim line))) of
      [x, y, z] -> (x, y, z)
    | _ -> error "bad vertex line in {path}: {line}") rows.
key p = (x, y, z) = p; "{x} {y} {z}".
# dict.fromkeys order: vertices by first appearance, ids in that order.
dedupe points =
  (m, n, rev) = List.fold (fn st p -> (m, n, rev) = st;
      case Map.get (key p) m of
        O.Some i -> (m, n, rev)
      | O.None -> (Map.insert (key p) n m, n + 1, p :: rev)) (Map.strings, 0, []) points;
  (List.rev rev, m).
idOf m p = case Map.get (key p) m of O.Some i -> i | O.None -> error "vertex lost".
triangles : unsafe Map.Map String Int -> List (Int, Int, Int) -> List String.
triangles m ps = case ps of
    a :: b :: c :: rest -> "<triangle v1=\"{idOf m a}\" v2=\"{idOf m b}\" v3=\"{idOf m c}\"/>" :: triangles m rest
  | _ -> [].
# re.sub(b'<mesh>.*?</mesh>', mesh, data, flags=re.S)
replaceMeshes : unsafe String -> String -> String.
replaceMeshes mesh data =
  i = Str.indexOf "<mesh>" data;
  if i < 0 then data else
    rest = N.drop (i + 6) data;
    j = Str.indexOf "</mesh>" rest;
    if j < 0 then data else N.take i data + mesh + replaceMeshes mesh (N.drop (j + 7) rest).

# ---- G-code -----------------------------------------------------------------
isDigitDot c = or (and (c >= 48) (c <= 57)) (c == 46).
spanEnd : unsafe String -> Int -> Int.
# `and` evaluates both operands, so bounds are tested before indexing.
spanEnd s i = if i >= Str.len s then i else if isDigitDot (Str.at s i) then spanEnd s (i + 1) else i.
# re.findall(r'([XYZE])(-?[\d.]+)', s), later keys winning as in the dict.
scan : unsafe String -> Int -> (Int, Int, Int, Int) -> (Int, Int, Int, Int).
scan s i acc = if i >= Str.len s then acc else
  c = Str.at s i;
  if not (List.has c [88,89,90,69]) then scan s (i + 1) acc else
    k = if i + 1 >= Str.len s then i + 1 else if Str.at s (i + 1) == 45 then i + 2 else i + 1;
    e = spanEnd s k;
    if e == k then scan s (i + 1) acc else
      v = pyFloat (Str.slice s (i + 1) e);
      (fx, fy, fz, fe) = acc;
      next = case c of 88 -> (v, fy, fz, fe) | 89 -> (fx, v, fz, fe) | 90 -> (fx, fy, v, fe) | _ -> (fx, fy, fz, v);
      scan s e next.
# Fast path: a match is only [XYZE]-?[0-9.]+, so matches never contain a space
# or another letter and each letter's value is its last valid occurrence. A
# space-separated token that is a letter and a plain number is decoded directly;
# any other shape (letters inside tokens, odd characters) re-reads the line with
# `scan`, the exact findall equivalent.
edge c = c >= 45 and c <= 57 and c != 47.
plainNumber r =
  n = Str.len r;
  if n == 0 then False else edge (Str.at r 0) and edge (Str.at r (n - 1)) and Str.lower r == Str.upper r.
# Python float() also takes ".5", "-.5" and "5."; Try.parseNum wants a digit
# on both sides of the point.
decode r =
  n = Str.len r;
  c0 = Str.at r 0;
  r1 = if c0 == 46 then "0" + r
       else if c0 != 45 then r
       else if n < 2 then r
       else if Str.at r 1 == 46 then "-0" + Str.slice r 1 n else r;
  if or (r == ".") (r == "-.") then (False, 0) else case Try.parseNum r1 of
    Ok v -> (True, v * 1.0)
  | Err e -> if Str.endsWith "." r1 then (case Try.parseNum (r1 + "0") of Ok v -> (True, v * 1.0) | Err e2 -> (False, 0)) else (False, 0).
hasLetter t = Str.contains "X" t or Str.contains "Y" t or Str.contains "Z" t or Str.contains "E" t.
token st t =
  (ok, x, y, z, e) = st;
  if not ok then st else
    n = Str.len t;
    c = if n == 0 then 0 else Str.at t 0;
    if c == 88 or c == 89 or c == 90 or c == 69 then
      r = Str.slice t 1 n;
      if r == "" then st
      else if not (plainNumber r) then (False, x, y, z, e) else
        (good, v) = decode r;
        if not good then (False, x, y, z, e)
        else if c == 88 then (True, v, y, z, e) else if c == 89 then (True, x, v, z, e)
        else if c == 90 then (True, x, y, v, e) else (True, x, y, z, v)
    else if hasLetter t then (False, x, y, z, e) else st.
fields : unsafe String -> (Int, Int, Int) -> (Int, Int, Int, Int).
fields code pos =
  (x, y, z) = pos;
  # The first token is the "G0".."G3" that isMove matched.
  (ok, fx, fy, fz, fe) = List.fold token (True, x, y, z, 0) (List.drop 1 (Str.split 32 code));
  if ok then (fx, fy, fz, fe) else scan code 0 (x, y, z, 0).
isMove line = List.any (fn p -> Str.startsWith p line) ["G0 ","G1 ","G2 ","G3 "].
integrate : unsafe Bool -> Int -> Int -> (Int, Int) -> List String -> (Int, Int).
integrate wing cs sn offsets lines =
  (o0, o1) = offsets;
  (pc, ps) = turn 2.5;
  step = fn st line ->
    (active0, x, y, z, w, m) = st;
    active1 = if Str.startsWith "; OBJECT_ID:" line then True else active0;
    active = if Str.startsWith "; FEATURE: Custom" line then False else active1;
    if not (isMove line) then (active, x, y, z, w, m) else
      semi = Str.indexOf ";" line;
      code = if semi < 0 then line else Str.slice line 0 semi;
      (xx, yy, zz, e) = fields code (x, y, z);
      if and active (and (or (xx != x) (yy != y)) (e > 0)) then
        aft = if wing then
                chord = cs * (Num.div (x + xx) 2 - 128 + o0) + sn * (Num.div (y + yy) 2 - 128 + o1);
                chord * pc + Num.div (z + zz) 2 * ps
              else (0 - sn) * (Num.div (x + xx) 2 - 128 + o0) + cs * (Num.div (y + yy) 2 - 128 + o1);
        (active, xx, yy, zz, w + e, m + aft * e)
      else (active, xx, yy, zz, w, m);
  (a, x, y, z, weight, moment) = List.fold step (False, 0.0, 0.0, 0.0, 0.0, 0.0) lines;
  (weight, moment).
reportedTotal lines = case List.filter (Str.startsWith "; total filament weight") lines of
    l :: _ -> pyFloat (Str.trim (List.fold (fn acc part -> part) "" (Str.split 58 l)))
  | [] -> error "no total filament weight in G-code".

# ---- one part ---------------------------------------------------------------
tail n s = if Str.len s > n then N.drop (Str.len s - n) s else s.
slicePart : unsafe String -> String -> String -> String -> String -> J.Json.
slicePart slicer root variant base name =
  job = "{base}/x1c-fuselage";
  presets = "{job}/presets";
  (cs, sn) = turn (turnDegrees name);
  points = List.map (fn p -> (x, y, z) = p; (cs * x - sn * y, sn * x + cs * y, z)) (stlPoints "{root}/{name}.stl");
  (x0, y0, z0) = points ! 0;
  (lo0, hi0, lo1, hi1) = List.fold (fn b p -> (a0, b0, a1, b1) = b; (x, y, z) = p;
      (Numeric.min a0 x, Numeric.max b0 x, Numeric.min a1 y, Numeric.max b1 y)) (x0, x0, y0, y0) points;
  o0 = Num.div (lo0 + hi0) 2;
  o1 = Num.div (lo1 + hi1) 2;
  (vertices, ids) = dedupe points;
  vs = Str.join "" (List.map (fn p -> (x, y, z) = p;
      "<vertex x=\"{N.fixed 6 (x - o0)}\" y=\"{N.fixed 6 (y - o1)}\" z=\"{N.fixed 6 z}\"/>") vertices);
  ts = Str.join "" (triangles ids points);
  out = tempDir "glider-slice-study";
  pkg = "{out}/pkg";
  src = "{out}/src";
  template = "{job}/glider-project.3mf";
  # Read members from one extraction and replace them in another: a file this
  # transaction has read must not then be rewritten under it.
  _ = checked ["unzip","-q",template,"-d",src];
  _ = checked ["unzip","-q",template,"-d",pkg];
  entries = List.filter (fn e -> e != "") (Str.lines (output ["unzip","-Z1",template]));
  shelled = variant != "";
  process = unwrap (J.parse (readPath "{presets}/process.json"));
  overrides = [("wall_loops","1"),("top_shell_layers","2"),("bottom_shell_layers","1"),("sparse_infill_density","5%")];
  process2 = List.fold (fn j kv -> (k, v) = kv; J.set k (J.JStr v) j) process overrides;
  processPath = if shelled then "{out}/process.json" else "{presets}/process.json";
  _ = if shelled then put processPath (dumps process2) else Unit;
  settingsPath = "{pkg}/Metadata/project_settings.config";
  _ = if and shelled (List.has "Metadata/project_settings.config" entries) then
        settings = unwrap (J.parse (readPath "{src}/Metadata/project_settings.config"));
        put settingsPath (dumps (List.fold (fn j kv -> (k, v) = kv; J.set k (unwrap (J.get k process2)) j) settings overrides))
      else Unit;
  modelPath = "{pkg}/3D/3dmodel.model";
  model = replaceMeshes "<mesh><vertices>{vs}</vertices><triangles>{ts}</triangles></mesh>" (readPath "{src}/3D/3dmodel.model");
  _ = if Str.contains "128.0000 128.0000 0" model then Unit else error "template plate centre missing in {template}";
  _ = put modelPath model;
  _ = case Proc.query (ProcessSpec (["zip","-q","-X","-0","-D","-nw","{out}/part.3mf"] + entries) pkg [] "" 0) of
      Ok (ProcessResult 0 _ _) -> Unit | other -> error "cannot pack {out}/part.3mf: {other}";
  log = "{out}/slicer.log";
  code = case Proc.streamNow (P.spec ["sh","-c","log=$1; shift; exec \"$@\" >\"$log\" 2>&1","slice-study",log,slicer,
      "--load-settings","{presets}/machine.json;{processPath}","--load-filaments","{presets}/filament.json",
      "--arrange","0","--orient","0","--slice","1","--outputdir",out,"{out}/part.3mf"]) of
      Ok c -> c | Err why -> error (str why);
  _ = if List.has "plate_1.gcode" (case Try.ls out of Ok names -> names | Err e -> []) then Unit
      else error "{name}: slicer exited {code} without plate_1.gcode\n{tail 2000 (readPath log)}";
  lines = Str.lines (readPath "{out}/plate_1.gcode");
  fea = case unwrap (Env.get "GLIDER_FEA_DIR") of EnvText d -> d | EnvMissing -> "";
  _ = if and shelled (and (name == "fuselage") (fea != "")) then
        _ = mkdirp fea;
        _ = writePath "{fea}/glider-{variant}-body.gcode" (Str.join "\n" lines);
        writePath "{fea}/glider-{variant}-offsets" "[{N.repr o0}, {N.repr o1}]"
      else Unit;
  reported = reportedTotal lines;
  (weight, moment) = integrate (Str.contains "wing" name) cs sn (o0, o1) lines;
  filament = unwrap (J.parse (readPath "{presets}/filament.json"));
  density = pyFloat (unwrap (J.path ["filament_density"] filament |>? J.at 0 |>? J.text));
  mass = Num.div (weight * N.pi * 0.765625 * density) 1000;
  _ = if Numeric.abs (mass - reported) < 1 then Unit else error "('{name}', {N.repr mass}, {N.repr reported})";
  J.JObj [("mass_g",J.JNum mass),("aft_mm",J.JNum (Num.div moment weight)),("slicer_total_g",J.JNum reported)].

# Python's sum() over floats (CPython 3.12+, ints included since 3.14):
# Neumaier-compensated addition, the compensation added once at the end.
neumaier st x =
  (f, c) = st;
  t = f + x;
  if Numeric.abs f >= Numeric.abs x then (t, c + ((f - t) + x)) else (t, c + ((x - t) + f)).
pySum xs = case xs of
    [] -> 0
  | x :: rest ->
    (f, c) = List.fold neumaier (x, 0.0) rest;
    if c != 0 then f + c else f.

# ---- study ------------------------------------------------------------------
# rel: the study folder from the repository root; variant: "" for the top-level
# study, else the folder name (process override + FEA export); parts: what to
# slice; seeded: start from shell/wing-study-slices.json; extras: (mass, y)
# for the fixed items the study adds.
main : unsafe String -> String -> List String -> Bool -> List (Int, Int) -> Unit.
main rel variant parts seeded extras =
  argv = args Unit;
  if or (List.has "--help" argv) (List.has "-h" argv) then print "usage: slice-study.sol [--slicer PATH]" else
    opts = unwrap (C.parse [C.Option "slicer" "" C.One] argv);
    _ = if C.operands opts == [] then Unit else error "usage: slice-study.sol [--slicer PATH]";
    slicer = C.valueOr defaultSlicer "slicer" opts;
    root = R.modelDir rel "slice-study.sol";
    light = if variant == "" then root else parent root;
    base = "{parent light}/bolted-glider-balanced";
    start = if seeded then unwrap (J.parse (readPath "{light}/shell/wing-study-slices.json")) else J.JObj [];
    result = List.fold (fn r name -> J.set name (slicePart slicer root variant base name) r) start parts;
    text = dumpsIndent "" result;
    _ = print text;
    _ = writePath "{root}/wing-study-slices.json" (text + "\n");
    baseline = unwrap (J.parse (readPath "{base}/balance.json"));
    studied = List.map (fn kv -> (k, v) = kv; (numAt "mass_g" v, numAt "aft_mm" v)) (unwrap (J.fields result));
    fixed = List.filter (fn v -> not (List.has (unwrap (J.get "name" v |>? J.text)) ["wing-left","wing-right","fuselage"]))
      (unwrap (J.get "items" baseline |>? J.items));
    hardware = List.map (fn v -> (numAt "mass_g" v, unwrap (J.get "cg_mm" v |>? J.at 1 |>? J.num))) fixed;
    items = studied + hardware + extras;
    total = pySum (List.map (fn it -> (m, y) = it; m) items);
    cg = Num.div (pySum (List.map (fn it -> (m, y) = it; m * y) items)) total;
    np = numAt "neutral_point_y_mm" baseline;
    mac = numAt "mac_mm" baseline;
    limit = np - 0.15 * mac;
    print "{N.repr total} {N.repr cg} {N.repr (Num.div (np - cg) mac)} aft ballast at 194: {N.repr (Num.div (total * (limit - cg)) (194 - limit))}".
