# Pin, check and run exactly the tool dependencies selected by the caller.
C = use "sol/lib/cli".
J = use "sol/lib/json".
L = use "std/list".
O = use "std/order".
P = use "sol/lib/proc".

scopes = ["source","build","render","slice","editor","printer","manual","test","infrastructure"].
env key fallback = case unwrap (Env.get key) of EnvMissing -> fallback | EnvText s -> if s == "" then fallback else s.
refuse why = _ = unwrap (IO.stderrText ("pipeline lock: " + why + "\n")); Script.exit 1.
must result = case result of Ok value -> value | Err why -> _ = refuse why; error why.
field name json = must (J.get name json).
string name json = must (J.get name json |>? J.text).
fields json = must (J.fields json).
keys json = List.map (fn pair -> (name,value) = pair; name) (fields json).
jsonFile path = must (Try.readPath path |>? J.parse).
readLock root =
  lock = jsonFile "{root}/pipeline.lock.json";
  if J.get "layout" lock == Ok (J.JNum 1) then
    pins = field "pins" lock; _ = fields pins; pins
  else _ = refuse "unsupported pipeline lock layout"; J.JObj [].
host root python mode input =
  spec = P.spec [python,"{root}/scripts/pipeline_lock_host.py",mode] |> P.withStdin input;
  must (P.output (Proc.query spec) |>? J.parse).

# A probe specifies the key in pipeline.lock.json and the host input it measures.
probe scope name kind key suffix arguments = (scope,J.JArr [J.JStr name,J.JStr scope,J.JStr kind,J.JStr key,J.JStr suffix,J.strs arguments]).
file scope name key suffix = probe scope name "file" key suffix [].
tree scope name key suffix = probe scope name "tree" key suffix [].
binary scope name arg = probe scope name "binary" name "" [arg].
probes = [
  probe "source" "fprisc" "repository" "fprisc" "" ["fprisc"],
  probe "source" "bambu_lan" "repository" "bambu_lan" "" ["3d-models"],
  probe "source" "coscad_source" "source" "" "" [],
  probe "test" "bosl2" "repository" "bosl2" "" ["BOSL2"],
  binary "build" "ghc" "--numeric-version",binary "build" "cabal" "--numeric-version",
  binary "build" "fpr_compiler" "--version",binary "build" "clang" "--version",
  binary "test" "openscad" "--version",binary "editor" "node" "--version",binary "editor" "npm" "--version",
  binary "build" "python" "--version",binary "test" "test_python" "--version",
  probe "manual" "pandoc" "optional-binary" "pandoc" "" ["--version"],
  probe "manual" "mandoc" "optional-binary" "mandoc" "" [],
  binary "render" "coscad_binary" "--version",probe "build" "host" "host" "" "" [],
  tree "build" "openssl_headers" "openssl" "include",file "build" "libssl" "openssl" "lib/libssl.3.dylib",
  file "build" "libcrypto" "openssl" "lib/libcrypto.3.dylib",file "build" "openssl_binary" "openssl" "bin/openssl",
  probe "build" "openssl" "version" "openssl" "bin/openssl" ["version"],file "build" "cabal_freeze" "" "cabal.project.freeze",
  tree "test" "openscad_bundle" "openscad_bundle" "",tree "slice" "bambustudio_bundle" "bambustudio_bundle" "",
  tree "slice" "bambu_profiles" "bambu_profiles" "",file "slice" "bambustudio" "bambustudio" "",
  probe "slice" "bambustudio_version" "plist" "bambustudio_bundle" "Contents/Info.plist" ["CFBundleShortVersionString"],
  file "editor" "npm_lock" "" "vscode/package-lock.json",tree "editor" "editor_modules" "" "vscode/node_modules",
  probe "editor" "npm_distribution" "npm-tree" "npm" "" [],probe "test" "python_packages" "packages" "test_python" "" [],
  file "printer" "native_host" "native_host" "",file "printer" "printer_config" "printer_config" "",
  file "printer" "printer_ca" "printer_ca" "",file "printer" "printer_libssl" "openssl" "lib/libssl.3.dylib",
  file "printer" "printer_libcrypto" "openssl" "lib/libcrypto.3.dylib",
  probe "printer" "deployed_source" "deployment" "native_receipt" "" [],
  file "infrastructure" "tailscale" "tailscale" "",probe "infrastructure" "tailscale_version" "version" "tailscale" "" ["version"],
  probe "infrastructure" "linode" "linode" "" "" []].
inventory root python paths selected =
  rows = List.filter (fn pair -> (scope,row) = pair; List.has scope selected) probes;
  request = J.JObj [("paths",paths),("probes",J.JArr (List.map (fn pair -> (scope,row) = pair; row) rows))];
  pins = host root python "inspect" (J.render request);
  if not (List.has "printer" selected) then pins else
    receipt = field "deployed_source" pins;
    _ = if string "binary_sha256" receipt == string "sha256" (field "native_host" pins) then Unit else
      refuse "deployed binary differs from its recorded source/build receipt";
    J.set "deployed_source" (J.JObj [("scope",J.JStr "printer"),("repository",J.JStr "fprisc"),
      ("revision",field "application_commit" receipt),("layout",J.JNum 1)]) pins.

# JSON object order is irrelevant, including the nested file and package maps.
canonical : unsafe J.Json -> J.Json.
canonical value = case value of
    J.JObj pairs -> J.JObj (pairs |> L.sortBy O.string (fn pair -> (key,v) = pair; key)
                                   |> List.map (fn pair -> (key,v) = pair; (key,canonical v)))
  | J.JArr items -> J.JArr (List.map canonical items)
  | other -> other.
changes : unsafe J.Json -> J.Json -> List String -> List String.
changes expected current selected =
  want = fields expected |> List.filter (fn pair -> (key,v) = pair; case J.get "scope" v |>? J.text of
            Ok scope -> List.has scope selected | Err missing -> False) |> J.JObj;
  names = L.sortWith O.string (L.unique (keys want + keys current));
  List.filter (fn name -> (J.get name want |> mapOk canonical) != (J.get name current |> mapOk canonical)) names.
fullCommit s = Str.len s == 40 and List.all (fn c -> (c >= 48 and c <= 57) or (c >= 97 and c <= 102)) (Str.codes s).
checkCI root python pins =
  config = jsonFile "{root}/ci-actions.lock.json";
  _ = if J.get "layout" config != Ok (J.JNum 1) then refuse "unsupported CI action lock layout" else Unit;
  actions = field "actions" config;
  _ = List.map (fn pair -> (name,row) = pair;
    if fullCommit (string "revision" row) then Unit else refuse "CI action needs a full commit: {name}") (fields actions);
  bosl = string "revision" (field "bosl2" pins);
  refs = must (J.items (host root python "ci" ""));
  _ = List.map (fn row -> case List.map (fn cell -> must (J.text cell)) (must (J.items row)) of
      [workflow,"action",name,revision] -> if J.path [name,"revision"] actions == Ok (J.JStr revision) then Unit
        else refuse "unpinned/mismatched CI action in {workflow}: {name}"
    | [workflow,"bosl2",name,revision] -> if revision == bosl then Unit else refuse "CI BOSL2 revision differs from pipeline lock"
    | [workflow,"tool",name,version] -> (case J.get name pins of Err missing -> Unit | Ok pin ->
        v = string "version" pin; want = if Str.startsWith "v" v then Str.slice v 1 (Str.len v) else v;
        if version == want then Unit else refuse "CI {name}-version differs from pipeline lock")
    | _ -> refuse "invalid CI reference") refs;
  Unit.

tool name = case name of
    "coscad" -> ("coscad_binary",["source","render"]) | "openscad" -> ("openscad",["test"])
  | "bambustudio" -> ("bambustudio",["slice"]) | "cabal" -> ("cabal",["source","build"])
  | "node" -> ("node",["editor"]) | "npm" -> ("npm",["editor"]) | "python" -> ("test_python",["source","test","slice"])
  | _ -> _ = refuse "run supports only named pinned tools"; ("",[]).
runTool selected paths argv = case argv of [] -> refuse "--run needs a named pinned tool" | name :: rest ->
  (key,needed) = tool name;
  if not (List.all (fn scope -> List.has scope selected) needed) then refuse "selected scopes omit required tool dependencies" else
    base = P.spec ([string key paths] + rest)
      |> P.withEnv "BAMBU_STUDIO" (string "bambustudio" paths) |> P.withEnv "BAMBU_PROFILES" (string "bambu_profiles" paths)
      |> P.withEnv "BAMBU_LAN_DIR" (string "bambu_lan" paths + "/bambu-lan") |> P.withEnv "PATH" (string "_tool_path" paths);
    spec = if List.has name ["openscad","cabal"] then
      base |> P.withEnv "COSCAD_OPENSCAD" (string "openscad" paths) |> P.withEnv "COSCAD_BOSL2" (string "bosl2" paths) else base;
    code = must (Proc.inheritNow spec);
    if code == 0 then Unit else Script.exit (if code < 0 then 256 + code else code).

pinCandidate : unsafe String -> String -> Unit.
pinCandidate root python =
  paths = host root python "paths" "";
  first = inventory root python paths scopes;
  second = inventory root python paths scopes;
  _ = if canonical first == canonical second then Unit else refuse "dependency changed during inventory; previous lock preserved";
  _ = checkCI root python first;
  revision = Str.trim (must (P.output (Proc.query (P.withTimeout 30000 (P.spec ["git","-C",root,"rev-parse","HEAD"])))));
  print (J.render (J.JObj [("repository",J.JStr "coscad-pipeline"),("layout",J.JNum 1),
    ("baseline_source_revision",J.JStr revision),("pins",first)])).

# All tokens after --run belong to the child, including its own --scope or --help.
splitRun : (argv : List String | measure argv) -> (List String,Bool,List String).
splitRun argv = case argv of [] -> ([],False,[]) | "--run" :: rest -> ([],True,rest)
  | word :: rest -> (before,present,after) = splitRun rest; (word :: before,present,after).
usage = "usage: pipeline-lock.sol [--scope SCOPE ...] (--pin | --check | --check-ci | --run TOOL [ARG ...])".
mainWith : unsafe String -> String -> List String -> Unit.
mainWith root python argv =
  (before,run,after) = splitRun argv;
  if List.any (fn a -> List.has a ["-h","--help"]) before then print usage else
    opts = case C.parse [C.Option "pin" "" C.Switch,C.Option "check" "" C.Switch,C.Option "check-ci" "" C.Switch,
                         C.Option "scope" "" C.Many] before of
       Ok value -> value | Err why -> _ = unwrap (IO.stderrText ("{why}\n{usage}\n")); _ = Script.exit 2; error why;
    chosen = C.values "scope" opts;
    selected = if chosen == [] then scopes else L.unique chosen;
    modes = List.filter (fn value -> value) [C.flag "pin" opts,C.flag "check" opts,C.flag "check-ci" opts,run];
    if List.len modes != 1 or C.operands opts != [] or not (List.all (fn scope -> List.has scope scopes) selected)
    then _ = unwrap (IO.stderrText (usage + "\n")); Script.exit 2
    else if C.flag "check-ci" opts then _ = checkCI root python (field "pins" (jsonFile "{root}/pipeline.lock.json"));
      print "CI action and BOSL2 pins hold"
    else if C.flag "pin" opts then
      if chosen != [] then refuse "--pin always captures the complete baseline" else
        status = must (Proc.streamNow (P.spec [python,"{root}/scripts/pipeline_lock_host.py","pin",env "SOL" "sol"]));
        if status == 0 then Unit else Script.exit status
    else
      expected = readLock root;
      paths = host root python "paths" "";
      current = inventory root python paths selected;
      drift = changes expected current selected;
      if drift != [] then refuse ("dependency drift: " + Str.join ", " drift + "; restore the pin or review an explicit --pin update") else
        _ = checkCI root python expected;
        _ = unwrap (IO.stdoutText ("Pipeline lock holds ({List.len (keys current)} pins)\n"));
        if run then runTool selected paths after else Unit.
> root = env "COSCAD_PIN_ROOT" ".";
  python = env "COSCAD_PIN_PYTHON" "python3";
  case args Unit of ["--capture-candidate"] -> pinCandidate root python | argv -> mainWith root python argv.
