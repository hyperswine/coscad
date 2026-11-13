# Finish the A1/P1S globe plates and threaded fit coupon without changing G-code.
C = use "sol/lib/cli".
D = use "sol/lib/digest".
J = use "sol/lib/json".
P = use "sol/lib/proc".
field name obj = unwrap (J.get name obj).
num name obj = unwrap (J.num (field name obj)).
check ok message = if ok then Unit else error message.
load path = unwrap (J.parse (unwrap (P.output (Proc.query (P.spec ["cat",path]))))).
hash path = unwrap (D.sha256File path).
intersection geometry name =
  check (num "maximum_intersection_mm3" (field name geometry) < 0.00001) "interference in {name}".
threadChecks root codeHash =
  geom = load "{root}/validation.json";
  _ = intersection geom "rotation";
  _ = intersection geom "thread_fit";
  paths = load "{root}/toolpath-validation.json";
  _ = check (field "passed" paths == J.JBool True and field "gcode_sha256" paths == J.JStr codeHash) "stale toolpath validation";
  _ = check (field "effective_support_on_build_plate_only" paths == J.JStr "1") "support must start on plate";
  _ = List.map (fn key -> check (field key paths == J.JArr []) "unexpected {key}")
        ["socket_support_intrusions","male_thread_support_intrusions","bearing_region_support"];
  _ = check (num "bearing_gap_crossing_segments" paths == 0 and num "support_segments_checked" paths > 0) "invalid bearing or support paths";
  roots = field "support_roots" paths;
  check (field "components_without_prior_support" roots == J.JArr [] and num "bed_components" roots > 0) "unsupported thread toolpaths".
checkSlice folder coupon =
  result = load "{folder}/result.json";
  _ = check (num "return_code" result == 0 and field "error_string" result == J.JStr "Success.") "slicing failed";
  plates = unwrap (J.items (field "sliced_plates" result));
  _ = check (List.len plates == 1) "expected one sliced plate";
  plate = plates ! 0;
  _ = check (field "warning_message" plate == J.JStr "") "slicer reported a warning";
  if coupon then check (List.len (unwrap (J.items (field "objects" plate))) == 2) "expected both coupon parts" else Unit.
finalize python root kind =
  (folder,stem,device,id) = case kind of
      "a1" -> ("slice","globe-a1","A1","N2S")
    | "p1s" -> ("slice","globe-p1s","P1S","C12")
    | "threaded" -> ("slice","globe-v2-p1s","P1S","C12")
    | "coupon" -> ("coupon-slice","thread-coupon-p1s","P1S","C12")
    | _ -> error "unknown globe plate: {kind}";
  scripts = if kind == "a1" then "{root}/../../scripts" else "{root}/../../../scripts";
  modelFile = "/Applications/BambuStudio.app/Contents/Resources/profiles/BBL/machine/Bambu Lab {device}.json";
  _ = check (field "model_id" (load modelFile) == J.JStr id) "unexpected vendor printer identifier";
  path = "{root}/{folder}";
  codeHash = hash "{path}/plate_1.gcode";
  archiveHash = hash "{path}/{stem}.gcode.3mf";
  receipts = if kind == "threaded" then ["{path}/result.json","{root}/validation.json","{root}/toolpath-validation.json"]
             else if kind == "coupon" then ["{path}/result.json"] else [];
  snapshots = List.map hash receipts;
  _ = if receipts == [] then Unit else checkSlice path (kind == "coupon");
  _ = if kind == "threaded" then threadChecks root codeHash else Unit;
  args = [python,"{scripts}/prepared_package_host.py","globe",root,kind,folder,stem,device,id,codeHash,archiveHash] + snapshots;
  case Proc.streamNow (P.spec args) of Ok 0 -> Unit | Ok n -> error "globe package validation exited {n}" | Err why -> error why.
> argv = args Unit;
  usage = "usage: finalize.sol [--python PATH] MODEL-DIR a1|p1s|threaded|coupon";
  if List.has "--help" argv or List.has "-h" argv then print usage else
    opts = unwrap (C.parse [C.Option "python" "" C.One] argv);
    case C.operands opts of [dir,kind] ->
      root = Str.trim (unwrap (P.output (Proc.query (P.spec ["realpath",dir]))));
      finalize (C.valueOr "python3" "python" opts) root kind
    | _ -> error usage.
