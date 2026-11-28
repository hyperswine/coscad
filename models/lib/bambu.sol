# Bambu Lab printers on the LAN, from Sol (2026-11-28). Replaces the per-job dispatch.py
# scripts, bambu_cooldown.py and the AGVN boat's build.py / dispatch_x2d.py.
#
#   Bambu = use "../../lib/bambu".        (relative to the importing script)
#
# Colour by tags. A model's parts.json says which filament (material + colour) each part
# prints in. `slice` turns that into a Bambu Studio assemble list: every part at the same plate
# position, one filament each, merged into one object, so no hand-made template project is
# needed. `plan` matches every filament to a loaded spool in the printer's live report (same
# material, nearest colour, the preset's tray_info_idx preferred), and from the spool works out
# the nozzle (X2D) and the ams_mapping. `dispatch` refuses to start when the sliced package's
# filament-to-nozzle map no longer matches the spools.
#
# Dispatch: printer idle and fault-free, spools match, FTPS upload read back by SHA-256, one
# project_file, then status until the first layer. print-run.json is written before the upload
# and after every step; if it exists the job is never sent again.
#
# Secrets: the access code comes from BAMBU_ACCESS_CODE_<PRINTER>, the printer's keys in
# coscad/.env, BAMBU_ACCESS_CODE in ~/.config/bambu/<printer>.env (mode 600), or BAMBU_ACCESS_CODE. It is never printed, written or put on a command line:
# mosquitto reads its options from a bash process substitution fed on stdin, curl reads its
# `user` line from stdin (-K -). curl checks the printer certificate against bbl-ca.pem with the
# serial number as the host name (--connect-to); that is the identity check before MQTT is used.
J = use "sol/lib/json".
P = use "sol/lib/proc".
D = use "sol/lib/digest".

# ---------------------------------------------------------------- printers
# physical: slicer extruder k (1-based) runs on physical extruder physical ! k.
printer name = case name of
    "x2d" -> {name = "x2d", host = "192.168.0.22", hostKey = "IP_ADDR", serial = "20P8BJ692700673",
              codeKeys = ["X2D_ACCESS_CODE", "ACCESS_CODE"], url = "ftp:///", md5 = True, ams2 = True,
              external = 255, externalOnly = [255], physical = [1, 0], fans = ["P2", "P10", "P3"]}
  | "x1c" -> {name = "x1c", host = "192.168.0.14", hostKey = "", serial = "00M09D550701167",
              codeKeys = ["X1C_ACCESS_CODE"], url = "file:///sdcard/", md5 = True, ams2 = False,
              external = 254, externalOnly = [], physical = [0], fans = ["P2", "P3"]}
  | "p1s" -> {name = "p1s", host = "192.168.0.27", hostKey = "", serial = "01P00A442400974",
              codeKeys = ["P1S_ACCESS_CODE"], url = "file:///sdcard/", md5 = False, ams2 = False,
              external = 254, externalOnly = [254], physical = [0], fans = ["P2", "P3"]}
  | "a1" -> {name = "a1", host = "192.168.0.63", hostKey = "", serial = "03919D4A1103804",
              codeKeys = ["A1_ACCESS_CODE"], url = "file:///sdcard/", md5 = True, ams2 = False,
              external = 254, externalOnly = [254], physical = [0], fans = []}
  | _ -> error "unknown printer '{name}' (x2d, x1c, p1s, a1)".

# ---------------------------------------------------------------- small helpers
nl = Str.fromCode 10.
run argv = unwrap (Proc.query (P.spec argv)).
out argv = Str.trim (unwrap (P.output (Proc.query (P.spec argv)))).
ok argv = ProcessResult code _ _ = run argv; code == 0.
repoRoot unused = out ["git", "rev-parse", "--show-toplevel"].
now unused = out ["date", "+%Y-%m-%dT%H:%M:%S%z"].
stamp unused = out ["date", "+%Y%m%d-%H%M%S"].
pause seconds = run ["sleep", str seconds].
exists path = ok ["test", "-e", path].
# Written by an external process, so the file lands now, not at the end of the transaction.
putNow path text = ProcessResult code _ err = unwrap (Proc.query (P.withStdin text (P.spec
    ["sh", "-c", "cat > \"$1.tmp\" && mv \"$1.tmp\" \"$1\"", "bambu-put", path])));
  if code == 0 then Unit else error "cannot write {path}: {err}".
readNow path = unwrap (P.output (Proc.query (P.spec ["cat", path]))).
envValue key = case unwrap (Env.get key) of EnvText v -> v | EnvMissing -> "".
unquote s = if Str.len s >= 2 and (Str.startsWith "\"" s and Str.endsWith "\"" s or Str.startsWith "'" s and Str.endsWith "'" s)
  then Str.slice s 1 (Str.len s - 1) else s.
dotenv path = case Try.readPath path of
    Ok text -> List.map (fn line -> i = Str.find 61 line;
        (Str.trim (Str.slice line 0 i), unquote (Str.trim (Str.slice line (i + 1) (Str.len line)))))
      (List.filter (fn line -> Str.contains "=" line and not (Str.startsWith "#" (Str.trim line))) (Str.lines text))
  | Err _ -> [].
lookup key pairs = List.fold (fn acc kv -> (k, v) = kv; if k == key then v else acc) "" pairs.
firstNonEmpty xs = List.fold (fn acc x -> if acc == "" then x else acc) "" xs.
has key j = case J.get key j of Ok _ -> True | Err _ -> False.
get key j = case J.get key j of Ok v -> v | Err _ -> J.JNull.
text key j = case J.get key j |>? J.text of Ok t -> t | Err _ -> "".
num key j = case J.get key j |>? J.num of Ok n -> n | Err _ -> 0 - 1.
items key j = case J.get key j |>? J.items of Ok xs -> xs | Err _ -> [].
toInt s = if s == "" then 0 - 1 else case Try.parseInt s of Ok n -> n | Err _ -> 0 - 1.
# 1-based position of x in xs (0 when absent); `!` itself is 0-based.
listIndex x xs = List.fold (fn acc i -> if acc < 1 and xs ! i == x then i + 1 else acc) 0 (List.range 0 (List.len xs - 1)).

host p = env = dotenv "{repoRoot Unit}/.env";
  firstNonEmpty [if p.hostKey == "" then "" else lookup p.hostKey env, p.host].
caFile unused = "{repoRoot Unit}/models/lib/bbl-ca.pem".
accessCode p =
  env = dotenv "{repoRoot Unit}/.env";
  own = dotenv "{envValue "HOME"}/.config/bambu/{p.name}.env";
  code = firstNonEmpty ([envValue "BAMBU_ACCESS_CODE_{Str.upper p.name}"] + List.map (fn k -> lookup k env) p.codeKeys +
    [lookup "BAMBU_ACCESS_CODE" own, envValue "BAMBU_ACCESS_CODE"]);
  if code == "" then error "no access code for {p.name}: set BAMBU_ACCESS_CODE_{Str.upper p.name}, {Str.join "/" p.codeKeys} in coscad/.env, or BAMBU_ACCESS_CODE in ~/.config/bambu/{p.name}.env" else code.

# ---------------------------------------------------------------- MQTT
mqttOptions p code = Str.join nl ["-h {host p}", "-p 8883", "-u bblp", "-P {code}", "--cafile {caFile Unit}", "--insecure", ""].
# $1 serial, $2 message, $3 messages to collect, $4 seconds; options arrive on stdin. Payloads are
# printed hex-encoded, one per line (the X2D pretty-prints its JSON across lines).
mqttScript = "o=$(cat; echo .); o=$\{o%.\}; mosquitto_sub -o <(printf '%s' \"$o\") -t \"device/$1/report\" -F %x -C \"$3\" -W \"$4\" & s=$!; sleep 1; mosquitto_pub -o <(printf '%s' \"$o\") -t \"device/$1/request\" -m \"$2\"; wait $s; exit 0".
exchange p code message count seconds =
  argv = ["bash", "-c", mqttScript, "bambu-mqtt", p.serial, message, str count, str seconds];
  ProcessResult status stdout err = unwrap (Proc.query (P.withStdin (mqttOptions p code) (P.spec argv)));
  List.fold (fn acc line -> case Hex.decode (Str.trim line) |>? Utf8.decode |>? J.parse of Ok j -> acc + [j] | Err _ -> acc) [] (Str.lines stdout).
pushall = "\{\"pushing\":\{\"sequence_id\":\"0\",\"command\":\"pushall\",\"version\":1,\"push_target\":1\}\}".
# The full report (the pushall reply carries the AMS block).
status p code =
  fulls = List.filter (fn m -> has "ams" (get "print" m) and has "gcode_state" (get "print" m)) (exchange p code pushall 8 20);
  case fulls of m :: _ -> get "print" m | [] -> error "no full status report from {p.name}".
summary report = J.JObj (List.map (fn k -> (k, get k report))
  ["gcode_state", "mc_percent", "layer_num", "total_layer_num", "mc_remaining_time", "subtask_name", "print_error", "fail_reason", "stg_cur", "hms"]).

# ---------------------------------------------------------------- FTPS (curl)
curlArgs p = ["curl", "-sS", "-K", "-", "--ssl-reqd", "--cacert", caFile Unit, "--connect-to", "{p.serial}:990:{host p}:990", "--max-time", "600"].
curlUser code = "user = \"bblp:{code}\"" + nl.
curl p code extra = unwrap (Proc.query (P.withStdin (curlUser code) (P.spec (curlArgs p + extra)))).
# The listing only works when the certificate chains to the BBL CA and names this serial.
identity p code = ProcessResult c _ err = curl p code ["--list-only", "ftps://{p.serial}:990/"];
  if c == 0 then Unit else error "{p.name} at {host p} did not prove it is {p.serial}: {err}".
remoteSha p code remote =
  ProcessResult c stdout err = unwrap (Proc.query (P.withStdin (curlUser code) (P.spec
    (["bash", "-c", "\"$@\" -o - | shasum -a 256", "bambu-get"] + curlArgs p + ["ftps://{p.serial}:990/{remote}"]))));
  case Str.words stdout of h :: _ -> h | [] -> "".

# ---------------------------------------------------------------- spools and colours
hexDigit c = if c >= 48 and c <= 57 then c - 48 else if c >= 97 and c <= 102 then c - 87 else if c >= 65 and c <= 70 then c - 55 else 0.
byteAt s i = 16 * hexDigit (Str.at s i) + hexDigit (Str.at s (i + 1)).
rgb s = t = if Str.startsWith "#" s then Str.slice s 1 (Str.len s) else s;
  if Str.len t < 6 then error "colour '{s}' is not RRGGBB" else (byteAt t 0, byteAt t 2, byteAt t 4).
distance2 a b = (r1, g1, b1) = rgb a; (r2, g2, b2) = rgb b;
  (r1 - r2) * (r1 - r2) + (g1 - g2) * (g1 - g2) + (b1 - b2) * (b1 - b2).
colourLimit = 6400.

# Every loaded feed: AMS trays, the X2D virtual slots, the older printers' vt_tray.
feeds report =
  units = items "ams" (get "ams" report);
  trays = List.fold (fn acc u -> unit = toInt (text "id" u);
      acc + List.map (fn t -> {unit = unit, slot = toInt (text "id" t), external = False, material = text "tray_type" t,
                               colour = text "tray_color" t, idx = text "tray_info_idx" t}) (items "tray" u)) [] units;
  virtual = List.map (fn v -> {unit = toInt (text "id" v), slot = 0, external = True, material = text "tray_type" v,
                               colour = text "tray_color" v, idx = text "tray_info_idx" v}) (items "vir_slot" report);
  vt = if has "vt_tray" report then [vtFeed (get "vt_tray" report)] else [];
  List.filter (fn f -> f.material != "") (trays + (if virtual == [] then vt else virtual)).
vtFeed v = {unit = 254, slot = 0, external = True, material = text "tray_type" v, colour = text "tray_color" v, idx = text "tray_info_idx" v}.
feedName f = if f.external then "external spool ({f.unit})" else "AMS {f.unit} slot {f.slot + 1}".
describe f = "{feedName f}: {f.material} {f.colour} {f.idx}".
same a b = a.unit == b.unit and a.slot == b.slot.

# Which slicer extruder (1-based) a feed sits on: the nozzle whose `snow` names it, else the
# printer's usual path (X2D: AMS on physical 1, external on physical 0).
nozzleOf p report f =
  code = f.unit * 256 + f.slot;
  info = items "info" (get "extruder" (get "device" report));
  loaded = List.fold (fn acc e -> if num "snow" e == code then num "id" e else acc) (0 - 1) info;
  phys = if loaded >= 0 then loaded else if List.len p.physical == 1 then 0 else if f.external then 0 else 1;
  k = listIndex phys p.physical;
  if k < 1 then error "{p.name}: no slicer extruder for physical nozzle {phys}" else k.

# filament: {key, material, colour ("" = any), idx ("" = any)}
choose avail used f =
  fits = List.filter (fn x -> Str.upper x.material == Str.upper f.material and not (List.any (same x) used)
                              and (f.colour == "" or distance2 x.colour f.colour <= colourLimit)) avail;
  score x = (if f.colour == "" then 0 else distance2 x.colour f.colour) - (if f.idx != "" and x.idx == f.idx then 100000 else 0);
  case fits of
    [] -> error "no loaded spool matches {f.key} ({f.material} {f.colour}); loaded: {Str.join "; " (List.map describe avail)}"
  | x :: rest -> List.fold (fn best y -> if score y < score best then y else best) x rest.
plan p report filaments =
  avail = feeds report;
  picks = List.fold (fn acc f -> acc + [choose avail acc f]) [] filaments;
  rows = List.map (fn pair -> (f, x) = pair;
      {key = f.key, feed = x, nozzle = nozzleOf p report x,
       mapping = if x.external then p.external else x.unit * 4 + x.slot}) (List.zip filaments picks);
  anyAms = List.any (fn r -> not r.feed.external) rows;
  {rows = rows, useAms = p.ams2 or anyAms,
   mapping = if p.ams2 or anyAms then List.map (fn r -> r.mapping) rows else p.externalOnly,
   filamentMap = Str.join " " (List.map (fn r -> str r.nozzle) rows)}.
planJson p pl = J.JObj [("filament_map", J.JStr pl.filamentMap), ("use_ams", J.JBool pl.useAms), ("ams_mapping", J.nums pl.mapping),
  ("spools", J.JArr (List.map (fn r -> J.JObj [("filament", J.JStr r.key), ("feed", J.JStr (describe r.feed)), ("nozzle", J.JNum r.nozzle)]) pl.rows))].
amsMapping2 pl = J.JArr (List.map (fn r -> J.JObj [("ams_id", J.JNum r.feed.unit), ("slot_id", J.JNum r.feed.slot)]) pl.rows).

# ---------------------------------------------------------------- tags (parts.json)
# {"filaments": [{"key", "material", "colour", "idx", "preset"}], "parts": [{"name", "filament"}],
#  "supports": key, "center": [x, y]}
# Presets are relative to the folder holding parts.json (tags carry it as "dir").
filamentsOf tags = List.map (fn f -> {key = text "key" f, material = text "material" f, colour = text "colour" f, idx = text "idx" f,
  preset = within (text "dir" tags) (text "preset" f)}) (items "filaments" tags).
within dir q = if Str.startsWith "/" q then q else "{dir}/{q}".
readTags dir = J.set "dir" (J.JStr dir) (unwrap (J.parse (readNow "{dir}/parts.json"))).
partsOf tags = List.map (fn q -> {name = text "name" q, filament = text "filament" q}) (items "parts" tags).
filamentIndex tags key = k = listIndex key (List.map (fn f -> f.key) (filamentsOf tags));
  if k < 1 then error "unknown filament '{key}' in parts.json" else k.

# ---------------------------------------------------------------- cool-down
anchor = ";=====printer finish air purification=========".
cooldownBlock p minutes = on = Str.join "" (List.map (fn f -> "M106 {f} S255" + nl) p.fans);
  off = Str.join "" (List.map (fn f -> "M106 {f} S0" + nl) p.fans);
  ";===== coscad rapid cool-down: {Str.join " " p.fans} at full speed for {minutes} min =====" + nl + on +
  "M400 S{minutes * 60}" + nl + off + ";===== coscad rapid cool-down end =====" + nl + nl.
# A copy of the machine preset whose end G-code runs the fans after the hotends are off.
withCooldown p machine minutes = if minutes <= 0 then machine else
  end = text "machine_end_gcode" machine;
  i = Str.indexOf anchor end;
  if p.fans == [] then error "{p.name} has no cool-down fans"
  else if i < 0 then error "machine end G-code has no '{anchor}' to anchor the cool-down"
  else J.set "machine_end_gcode" (J.JStr (Str.slice end 0 i + cooldownBlock p minutes + Str.slice end i (Str.len end))) machine.

# ---------------------------------------------------------------- slicing
slicer = "/Applications/BambuStudio.app/Contents/MacOS/BambuStudio".
# job: {dir, tags, stls (part name -> STL), machine, process, center (x, y), filamentMap, name}
# Bambu Studio takes the manual nozzle map from plate_params "filament_map" (comma separated).
assembleList job = (cx, cy) = job.center;
  objects = List.map (fn q -> J.JObj [("path", J.JStr (lookup q.name job.stls)), ("count", J.JNum 1), ("subtype", J.JStr "normal_part"),
      ("filaments", J.nums [filamentIndex job.tags q.filament]), ("assemble_index", J.nums [1]),
      ("pos_x", J.nums [cx]), ("pos_y", J.nums [cy]), ("pos_z", J.nums [0])]) (partsOf job.tags);
  params = J.JObj (if job.filamentMap == "" then [] else [("filament_map_mode", J.JStr "Manual"), ("filament_maps", J.JStr job.filamentMap),
      ("filament_map", J.JStr (Str.replace " " "," job.filamentMap))]);
  J.JObj [("plates", J.JArr [J.JObj [("plate_name", J.JStr job.name), ("need_arrange", J.JBool False), ("plate_params", params), ("objects", J.JArr objects)]])].
slice job =
  sliceDir = "{job.dir}/slice";
  _ = run ["mkdir", "-p", sliceDir];
  listPath = "{job.dir}/assemble-list.json";
  _ = putNow listPath (J.pretty (assembleList job) + nl);
  presets = Str.join ";" (List.map (fn f -> f.preset) (filamentsOf job.tags));
  ProcessResult code _ err = run ["sh", "-c", "log=$1; shift; exec \"$@\" >\"$log\" 2>&1", "bambu-slice", "{sliceDir}/slicer.log",
      slicer, "--debug", "2", "--load-settings", "{job.machine};{job.process}", "--load-filaments", presets,
      "--load-assemble-list", listPath, "--arrange", "0", "--orient", "0", "--slice", "1",
      "--export-3mf", "{job.name}.gcode.3mf", "--outputdir", sliceDir];
  result = unwrap (J.parse (readNow "{sliceDir}/result.json"));
  if code != 0 then error "Bambu Studio exited {code} ({text "error_string" result}); see {sliceDir}/slicer.log" else
  plate = case items "sliced_plates" result of x :: _ -> x | [] -> error "no sliced plate";
  gcode = "{sliceDir}/plate_1.gcode";
  count pattern = toInt (out ["sh", "-c", "grep -c -E \"$1\" \"$2\" || true", "bambu-count", pattern, gcode]);
  setting key = out ["sh", "-c", "grep -m1 \"^; $1 = \" \"$2\" | sed 's/^[^=]*= //'", "bambu-setting", key, gcode];
  J.JObj [("package", J.JStr "{sliceDir}/{job.name}.gcode.3mf"), ("hours", J.JNum (Num.div (Num.round (Num.div (num "total_predication" plate) 36)) 100)),
    ("grams", J.JArr (List.map (fn f -> J.JNum (Num.div (Num.round (10 * (case J.get "total_used_g" f |>? J.num of Ok g -> g | Err _ -> 0))) 10)) (items "filaments" plate))),
    ("warning", get "warning_message" plate), ("filament_map", J.JStr (setting "filament_map")),
    ("tool_changes", J.JNum (count "^T[0-9]( |$)")), ("support_paths", J.JNum (count "^; FEATURE: .*[Ss]upport")),
    ("prime_tower_paths", J.JNum (count "^; FEATURE: Prime tower")), ("cooldown", J.JBool (count "coscad rapid cool-down" > 0))].

# ---------------------------------------------------------------- dispatch
pkgSetting package key = out ["sh", "-c", "unzip -p \"$1\" Metadata/plate_1.gcode | grep -m1 \"^; $2 = \" | sed 's/^[^=]*= //'", "bambu-pkg", package, key].
pkgFilaments package = toInt (out ["sh", "-c", "unzip -p \"$1\" Metadata/slice_info.config | grep -c '<filament id' || true", "bambu-pkg", package]).
idle report = List.has (text "gcode_state" report) ["FINISH", "IDLE", "FAILED"] and num "print_error" report == 0 and items "hms" report == [].
checkIdle p report = if idle report then Unit else
  error "{p.name} is not ready: state {text "gcode_state" report}, print_error {num "print_error" report}, hms {J.render (get "hms" report)}".
# Same filaments in the same order, and on the X2D the same nozzle for each.
checkPackage p pl package filaments =
  n = pkgFilaments package;
  sliced = Str.replace "," " " (pkgSetting package "filament_map");
  if n != List.len filaments then error "package has {n} filaments, the job names {List.len filaments}"
  else if List.len p.physical > 1 and sliced != pl.filamentMap then
    error "the spools need filament_map '{pl.filamentMap}' but the package was sliced with '{sliced}': re-slice with the current spools"
  else Unit.

uploadFailed record r why =
  u = putNow record (J.pretty (J.set "phase" (J.JStr "upload-failed") (J.set "upload_error" (J.JStr why) r)) + nl);
  error "upload not verified ({why})".
# job: {dir, printer, package, remote, filaments, bed, options : List (String, Json)}
startCommand p job pl remote md5 =
  base = [("sequence_id", J.JStr (out ["date", "+%s"])), ("command", J.JStr "project_file"), ("param", J.JStr "Metadata/plate_1.gcode"),
    ("url", J.JStr (p.url + remote)), ("subtask_name", J.JStr (Str.replace ".gcode.3mf" "" remote)),
    ("project_id", J.JStr "0"), ("profile_id", J.JStr "0"), ("task_id", J.JStr "0"), ("subtask_id", J.JStr "0"),
    ("md5", J.JStr (if p.md5 then md5 else "")), ("bed_type", J.JStr job.bed), ("bed_levelling", J.JBool True), ("timelapse", J.JBool False)]
    + (if p.ams2 then [] else [("flow_cali", J.JBool True), ("vibration_cali", J.JBool True), ("layer_inspect", J.JBool True)])
    + [("use_ams", J.JBool pl.useAms), ("ams_mapping", J.nums pl.mapping)]
    + (if p.ams2 then [("ams_mapping2", amsMapping2 pl)] else []);
  List.fold (fn acc kv -> (k, v) = kv; J.set k v acc) (J.JObj base) job.options.

dispatch job confirmed note dryRun =
  p = printer job.printer;
  record = "{job.dir}/print-run.json";
  _ = if exists record and not dryRun then error "{record} exists: this job was dispatched; check the printer instead of resending" else Unit;
  _ = if exists job.package then Unit else error "no package at {job.package}";
  code = accessCode p;
  _ = identity p code;
  report = status p code;
  _ = if dryRun then print "state: {J.render (summary report)}" else checkIdle p report;
  pl = plan p report job.filaments;
  _ = checkPackage p pl job.package job.filaments;
  _ = print "spools: {J.render (planJson p pl)}";
  if dryRun then print "dry run: spools and package match; {p.name} is {text "gcode_state" report}; would send {J.render (startCommand p job pl job.remote (out ["md5", "-q", job.package]))}; nothing sent" else
  _ = if confirmed then Unit else error "pass --confirm-bed-clear after the owner confirms the bed is clear";
  sha = unwrap (D.sha256File job.package);
  md5 = out ["md5", "-q", job.package];
  remote = job.remote;
  rec0 = J.JObj [("printer", J.JStr p.name), ("serial", J.JStr p.serial), ("package", J.JStr job.package), ("package_sha256", J.JStr sha),
    ("package_md5", J.JStr md5), ("remote_name", J.JStr remote), ("bed_clear_owner_confirmed", J.JBool True), ("authorization_note", J.JStr note),
    ("before", summary report), ("plan", planJson p pl), ("phase", J.JStr "preflight"), ("dispatch_attempted", J.JBool False),
    ("events", J.JArr []), ("created_at", J.JStr (now Unit))];
  save r = putNow record (J.pretty r + nl);
  _ = save rec0;
  ProcessResult upc _ uperr = curl p code ["-T", job.package, "ftps://{p.serial}:990/{remote}"];
  back = remoteSha p code remote;
  _ = if back == sha then Unit else uploadFailed record rec0 "curl {upc}: {uperr}";
  rec1 = J.set "phase" (J.JStr "uploaded-verified") (J.set "remote_content_sha256" (J.JStr back) rec0);
  _ = save rec1;
  again = status p code;
  _ = checkIdle p again;
  cmd = startCommand p job pl remote md5;
  rec2 = J.set "command" cmd (J.set "dispatch_attempted" (J.JBool True) (J.set "phase" (J.JStr "start-intent") rec1));
  _ = save rec2;
  replies = List.filter (fn m -> text "command" (get "print" m) == "project_file") (exchange p code (J.render (J.JObj [("print", cmd)])) 60 35);
  reply = case replies of m :: _ -> get "print" m | [] -> J.JNull;
  rec3 = J.set "reply" reply (J.set "phase" (J.JStr (if replies == [] then "no-acknowledgement" else "acknowledged")) rec2);
  _ = save rec3;
  _ = print "reply: {J.render reply}";
  subtask = Str.replace ".gcode.3mf" "" remote;
  final = List.fold (fn acc i -> (r, done) = acc;
      if done then acc else
      _ = pause 15;
      s = status p code;
      sm = summary s;
      ours = text "subtask_name" s == subtask;
      events = J.JArr (items "events" r + [J.JObj [("at", J.JStr (now Unit)), ("status", sm)]]);
      bad = num "print_error" s != 0 or items "hms" s != [] or (ours and List.has (text "gcode_state" s) ["FAILED", "PAUSE"]);
      first = ours and num "layer_num" s >= 1;
      r2 = J.set "events" events (if bad then J.set "phase" (J.JStr "attention") r else if first then J.set "phase" (J.JStr "first-layer") r else r);
      _ = save r2;
      _ = print (J.render sm);
      (r2, bad or first)) (rec3, False) (List.range 1 60);
  (r, _) = final;
  print "phase: {text "phase" r}".

# Command line for a job script: [--dry-run] [--confirm-bed-clear --note TEXT]
argAfter flag argv = List.fold (fn acc i -> if argv ! i == flag then argv ! (i + 1) else acc) "" (List.range 0 (List.len argv - 2)).
main job = argv = args Unit;
  dispatch job (List.has "--confirm-bed-clear" argv) (argAfter "--note" argv) (List.has "--dry-run" argv).
