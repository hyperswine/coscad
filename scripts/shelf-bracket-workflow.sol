# Reproduce the shelf bracket stress, manufacturing and instruction workflow.
C = use "sol/lib/cli".
D = use "sol/lib/digest".
J = use "sol/lib/json".
P = use "sol/lib/proc".
B = use "sol/lib/base".
L = use "std/list".
O = use "std/order".

sources = ["shelf-bracket.coscad","shelf-bracket.assemble","shelf-bracket-process.json"].
tools = ["scripts/shelf-bracket-workflow.sol","scripts/bambu-slice.py","scripts/pipeline-lock.py",
         "scripts/pipeline-lock.sol","scripts/pipeline_lock_host.py","pipeline.lock.json"].
native value = List.has value [J.JStr "native",J.JStr "native-experimental"].
query argv = P.output (Proc.query (P.spec argv)).
jsonFile path = Try.readPath path |>? J.parse.
snapshot dir names =
  List.fold (fn result name -> result |>? (fn rows ->
    D.sha256File "{dir}/{name}" |> mapOk (fn hash -> rows + [(name,J.JStr hash)]))) (Ok []) names |> mapOk J.JObj.

# Transactional writes would disappear on a failed stage. Publish the full
# record before starting a child and again as soon as its outcome is known.
publish path contents =
  pipe = PipelineSpec [P.spec ["cat"]] (PipeBytes (unwrap (Utf8.encode contents)))
      (PipePublish path True) PipeOutputInherit 0 PipeFail;
  case Proc.pipeline pipe of
    Ok (PipelineResult 0 _ _ _) -> Unit
  | Ok (PipelineResult code _ _ _) -> error "cannot write {path}: exit {code}"
  | Err why -> error (str why).
save out record = publish "{out}/workflow.json" (J.pretty record + "\n").
fail out record why =
  _ = save out (record |> J.set "status" (J.JStr "failed") |> J.set "error" (J.JStr why));
  error why.
require out record result = case result of Ok value -> value | Err why -> fail out record why.

# Keep stdout and stderr on disk throughout the run. The stage log follows
# the Python order (stdout, then stderr); capabilities parses only stdout.
stage out scratch record name argv seconds =
  entry = J.JObj [("name",J.JStr name),("command",J.strs argv),("status",J.JStr "running")];
  previous = unwrap (J.get "stages" record |>? J.items);
  withEntry e = J.set "stages" (J.JArr (previous + [e])) record;
  _ = save out (withEntry entry);
  _ = print name;
  stdout = "{scratch}/{name}.stdout";
  stderr = "{scratch}/{name}.stderr";
  redirected = ["sh","-c","err=$1; shift; exec \"$@\" 2>\"$err\"","shelf-stage",stderr] + argv;
  cmd = PipelineSpec [P.inDir out (P.spec redirected)] (PipeBytes (Bytes.empty Unit))
        (PipeFileNew stdout) PipeOutputInherit (seconds * 1000) PipeFail;
  start = Sys.mtime Unit;
  outcome = Proc.pipeline cmd;
  elapsed = Numeric.div ((Sys.mtime Unit - start + 50000) / 100000) 100;
  logName = "{name}.log";
  logSpec = PipelineSpec [P.spec ["cat",stdout,stderr]] (PipeBytes (Bytes.empty Unit))
       (PipePublish "{out}/{logName}" True) PipeOutputInherit 0 PipeFail;
  timed = entry |> J.set "seconds" (J.JNum elapsed);
  finishFailed e why = fail out (withEntry (e |> J.set "status" (J.JStr "failed") |> J.set "error" (J.JStr why))) why;
  case outcome of
    Err why -> finishFailed timed (str why)
  | Ok (PipelineResult code _ _ _) ->
      case Proc.pipeline logSpec of
        Ok (PipelineResult 0 _ _ _) ->
          logged = timed |> J.set "exit" (J.JNum code) |> J.set "log" (J.JStr logName);
          if code != 0 then finishFailed logged "{name} failed (exit {code}); see {out}/{logName}"
          else done = withEntry (J.set "status" (J.JStr "passed") logged); _ = save out done; done
      | other -> finishFailed timed "could not write stage log {out}/{logName}".

inspectManufacturing out =
  # Extract just the backend from the multi-megabyte element field. jq also
  # validates the complete report without loading that field into the Sol VM.
  query ["jq","-es","if length == 1 then .[0].geometry_backend else error(\"expected one report\") end","{out}/shelf-bracket_stress.json"] |>? J.parse |>? (fn backend ->
  jsonFile "{out}/shelf-bracket_manifest.json" |>? (fn manifest ->
  J.get "variants" manifest |>? J.items |>? (fn variants -> case variants of
    [v] -> if and (J.get "down" v == Ok (J.JStr "bot")) (J.path ["hints","material"] v == Ok (J.JStr "pla"))
           then J.get "geometry_backend" manifest |>? (fn mb -> if and (native mb) (native backend) then Ok Unit else Err "stress and manufacturing backend mismatch")
           else Err "manufacturing orientation/material differs from this analysis workflow"
  | _ -> Err "manufacturing orientation/material differs from this analysis workflow"))).

verifyPackage archive process scratch =
  jsonFile process |>? (fn settings ->
  expected = settings |> J.set "brim_type" (J.JStr "outer_only") |> J.set "brim_width" (J.JStr "5")
                      |> J.set "printer_settings_id" (J.JStr "Bambu Lab A1 0.4 nozzle");
  query ["unzip","-p",archive,"Metadata/project_settings.config"] |>? J.parse |>? (fn exported ->
  J.fields expected |>? (fn fields ->
  bad = List.filter (fn pair -> (key,value) = pair; J.get key exported != Ok value) fields;
  case bad of
    (key,value) :: rest -> Err "slicer changed {key}: expected {J.render value}"
  | [] ->
      file = "{scratch}/toolpaths.gcode";
      pipe = PipelineSpec [P.spec ["unzip","-p",archive,"Metadata/plate_1.gcode"]] (PipeBytes (Bytes.empty Unit))
             (PipeFileNew file) PipeOutputInherit 0 PipeFail;
      case Proc.pipeline pipe of
        Ok (PipelineResult 0 _ _ _) ->
          if and (P.succeeded (Proc.query (P.spec ["grep","-Fq","; FEATURE: Support",file])))
                 (P.succeeded (Proc.query (P.spec ["grep","-Fq","G1 ",file])))
          then Ok expected else Err "sliced toolpaths do not contain the expected supports and movement"
      | other -> Err "cannot read Metadata/plate_1.gcode from {archive}"))).

summary status = Str.join "\n" [
  "# Shelf bracket workflow","","Status: {status}. No printer command was sent.","",
  "- [Stage results and artifact hashes](workflow.json)",
  "- [Stress summary](stress.log) and [full stress field](shelf-bracket_stress.json)",
  "- [Assembly instructions](shelf-bracket_plan.md)",
  "- [Manufacturing manifest](shelf-bracket_manifest.json)","",
  "The 20 N calculation uses homogeneous PLA and fully clamped supports. It does not certify the printed bracket, mounting hardware, infill, layer bonding or creep. The print keeps model Z vertical, uses 100% infill and supports under the arm; supports may grow on the base. Review the sliced toolpaths before any dispatch.",""].

shelfWorkflow root out bin opts =
  scratch = tempDir "coscad-shelf";
  _ = List.map (fn name -> case Fs.copyPublishNow "{root}/examples/stress/{name}" "{out}/{name}" 65536 False of
       Ok bytes -> Unit | Err why -> error (str why)) sources;
  seed = J.JObj [("schema",J.JStr "coscad.workflow/1"),("example",J.JStr "shelf-bracket"),("backend",J.JStr "native"),
    ("compiler",J.JObj [("path",J.JStr bin),("sha256",J.JStr (unwrap (D.sha256File bin)))]),
    ("sources",unwrap (snapshot out sources)),("tools",unwrap (snapshot root tools)),
    ("status",J.JStr "running"),("dispatched",J.JBool False),("stages",J.JArr [])];
  step = stage out scratch;
  slicing = C.flag "slice" opts;
  python = C.valueOr "python3" "python" opts;
  initial = if slicing then step seed "slice-pins" [python,"{root}/scripts/pipeline-lock.py","--scope","slice","--check"] 300 else seed;
  cap = step initial "capabilities" [bin,"capabilities","--json"] 300;
  features = require out cap (jsonFile "{scratch}/capabilities.stdout");
  capable = J.set "capabilities" features cap;
  backends = require out capable (J.get "backends" features |>? J.items);
  _ = if List.any native backends then Unit else fail out capable "this compiler has no native geometry kernel";
  part = "{out}/shelf-bracket.coscad";
  asm = "{out}/shelf-bracket.assemble";
  analyzed = capable |> (fn r -> step r "validate-part" [bin,"validate","--json",part] 300)
      |> (fn r -> step r "validate-assembly" [bin,"validate","--json",asm] 300)
      |> (fn r -> step r "check" [bin,"check",asm] 300)
      |> (fn r -> step r "stress" [bin,"stress",part,"--full"] 900)
      |> (fn r -> step r "next" [bin,"next",asm] 300);
  _ = require out analyzed (inspectManufacturing out);
  plan = step analyzed "plan" ([bin,"plan",asm] + (if C.flag "images" opts then ["--png"] else [])) 300;
  process = "{out}/shelf-bracket-process.json";
  result = if slicing then
      sliced = step plan "slice" [python,"{root}/scripts/bambu-slice.py","{out}/shelf-bracket_manifest.json",
                    "--printer","Bambu Lab A1 0.4 nozzle","--process-overrides",process] 600;
      J.set "verified_slice_settings" (require out sliced (verifyPackage "{out}/shelf-bracket_bed1.gcode.3mf" process scratch)) sliced
    else plan;
  files = require out result (query ["find",out,"-maxdepth","1","-type","f","!","-name","workflow.json","-print0"]);
  names = Str.split 0 files |> List.filter (fn path -> path != "") |> List.map B.baseName |> L.sortWith O.string;
  hashes = require out result (snapshot out names);
  status = if slicing then "sliced" else "planned";
  _ = publish "{out}/README.md" (summary status);
  _ = save out (result |> J.set "status" (J.JStr status) |> J.set "artifacts" hashes);
  print "Completed: {out}/README.md".

> opts = unwrap (C.parse [C.Option "coscad" "" C.One,C.Option "output" "" C.One,C.Option "slice" "" C.Switch,
                          C.Option "images" "" C.Switch,C.Option "python" "" C.One] (args Unit));
  requested = C.valueOr "" "output" opts;
  _ = if or (requested == "") (C.operands opts != []) then error "usage: shelf-bracket-workflow.sol --output NEW-DIR [--coscad PATH] [--images] [--slice] [--python PATH]" else Unit;
  root = Str.trim (unwrap (query ["git","rev-parse","--show-toplevel"]));
  which = Str.trim (unwrap (query ["which",C.valueOr "coscad" "coscad" opts]));
  bin = Str.trim (unwrap (query ["realpath",which]));
  parent = Str.trim (unwrap (query ["dirname",requested]));
  _ = unwrap (query ["mkdir","-p",parent]);
  _ = unwrap (query ["mkdir",requested]);
  out = Str.trim (unwrap (query ["realpath",requested]));
  shelfWorkflow root out bin opts.
