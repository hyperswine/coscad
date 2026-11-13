# Shared balanced-glider provenance. Keys are model-relative POSIX paths.
D = use "sol/lib/digest".
B = use "sol/lib/base".
J = use "sol/lib/json".
P = use "sol/lib/proc".
L = use "std/list".
O = use "std/order".
checked argv = case Proc.streamNow (P.spec argv) of
    Ok 0 -> Unit
  | Ok code -> error "exit {code}: {argv}"
  | Err why -> error (str why).
output argv = unwrap (P.output (Proc.query (P.spec argv))).
line s = if Str.endsWith "\n" s then Str.slice s 0 (Str.len s - 1) else s.
modelRoot u =
  cwd = line (output ["pwd","-P"]);
  root = if exists "{cwd}/models/bolted-glider-balanced/build.sol"
         then "{cwd}/models/bolted-glider-balanced" else cwd;
  if and (exists "{root}/provenance.sol") (exists "{root}/design.py") then root
  else error "run from the CoScad root or the balanced-glider model directory".
# The generator and compiler add files to this directory during a build.
# Read its live listing so those expected writes do not invalidate a Sol snapshot.
sourceFiles dir =
  paths = output ["find",dir,"-maxdepth","1","-name","*.coscad","-type","f","-print0"];
  Str.split 0 paths |> List.filter (fn path -> path != "") |> List.map B.baseName |> L.sortWith O.string.
meshName source = Str.slice source 0 (Str.len source - 7) + ".stl".
geometryFiles sources = List.concat (List.map (fn src -> [src,meshName src]) sources).
hash root path = unwrap (D.sha256File "{root}/{path}").
row root path = (path,J.JStr (hash root path)).
entries path =
  rows = unwrap (J.fields (unwrap (J.parse (readPath path))));
  names = List.map (fn item -> (name,value) = item; name) rows;
  if List.len (L.unique names) == List.len names then rows else error "duplicate input in {path}".
safeKey key = List.all (fn part -> not (List.has part ["",".",".."])) (Str.split 47 key).
requireWith allowed root rows =
  _ = List.map (fn pair -> (path,value) = pair;
    if not (safeKey path or List.has path allowed) then error "invalid input path: {path}"
    else if hash root path == unwrap (J.text value) then Unit
    else error "Stale input: {path}; rebuild before using the report/package") rows;
  Unit.
require root rows = requireWith ["../../scripts/prepare-projects.sol","../../scripts/prepare_project_host.py"] root rows.
requireKeys keys rows =
  names = List.map (fn item -> (name,value) = item; name) rows;
  _ = List.map (fn key ->
    if List.has key names then Unit else error "missing provenance entry: {key}") keys;
  Unit.
# Publish complete JSON immediately so a later child process can read it.
publish path rows =
  bytes = unwrap (Utf8.encode (J.pretty (J.JObj rows) + "\n"));
  spec = PipelineSpec [P.spec ["cat"]] (PipeBytes bytes) (PipePublish path True) PipeOutputInherit 0 PipeFail;
  case Proc.pipeline spec of
    Ok (PipelineResult 0 _ _ _) -> Unit
  | Ok (PipelineResult code _ _ _) -> error "could not publish {path}: {code}"
  | Err why -> error (str why).
copyFile src dest = case Fs.copyPublishNow src dest 65536 True of
    Ok bytes -> Unit
  | Err why -> error (str why).

# Both the balanced glider and Orbit Tower use the glider-project.3mf archive layout.
# Publish the staged outputs only after the model has checked its source receipts again.
sliceProject root job slicer inputs drivers recheck =
  folder = "{root}/{job}";
  stage = tempDir "coscad-glider-slice";
  presets = "{folder}/presets";
  out = "{folder}/slice";
  _ = checked ["mkdir","-p",out];
  argv = ["sh","-c","log=$1; shift; exec \"$@\" >\"$log\" 2>&1","coscad-slice","{out}/slicer.log",slicer,
    "--debug","2","--load-settings","{presets}/machine.json;{presets}/process.json",
    "--load-filaments","{presets}/filament.json","--arrange","0","--orient","0","--slice","1",
    "--export-3mf","glider.gcode.3mf","--outputdir",stage,"{folder}/glider-project.3mf"];
  _ = checked argv;
  digest = hash stage "plate_1.gcode";
  _ = hash stage "glider.gcode.3mf";
  _ = hash stage "result.json";
  _ = recheck Unit;
  _ = List.map (fn name -> copyFile "{stage}/{name}" "{out}/{name}") (ls stage);
  # A Sol-prepared bed can already name the shared provenance module.
  driverPaths = List.map (fn item -> (name,value) = item; name) drivers;
  prepared = List.filter (fn item -> (name,value) = item; not (List.has name driverPaths)) inputs;
  _ = publish "{out}/input-hashes.json" (prepared + [("{job}/slice/plate_1.gcode",J.JStr digest)] + drivers);
  print "Sliced {job}".
