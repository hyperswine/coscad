# Batch native FEA comparisons with the independent scikit-fem element kernels.

B = use "sol/lib/base".
C = use "sol/lib/cli".
J = use "sol/lib/json".
P = use "sol/lib/proc".

beam = [["axial","4","2","2"],["bend","10","2","2"],["bend","20","4","4"],["bend","30","6","6"]].
cad =
  bracket level = ["experiments/cad/l-bracket.coscad","experiments/cad/l-bracket-{level}.json","bracket"];
  List.map bracket ["coarse","medium","fine"] + [["examples/assemble/constraints/post.coscad","experiments/cad/existing-post.json","post"]].

integer n = case Try.parseInt n of Ok cell -> str cell | Err why -> error "invalid cell count: {n}".

fixtures family cells = case family of
    "hex20" ->
      levels = if cells == [] then ["1","2","3"] else List.map integer cells;
      List.map (fn mode -> List.map (fn n -> [mode,n]) levels) ["axial","bend"] |> List.concat
  | "beam" -> if cells == [] then beam else error "cell overrides require hex20"
  | "cad" -> if cells == [] then cad else error "cell overrides require hex20"
  | _ -> error "unknown reference: {family}".

runCase exe host family tmp entry =
  (index,params) = entry;
  source = "{tmp}/actual-{index}.json";
  target = "{tmp}/case-{index}.json";
  argv = [exe,family] + (if family == "cad" then List.take 2 params else params);
  limit = case family of "beam" -> 60000 | "cad" -> 120000 | _ -> 600000;
  # CAD nodal arrays can exceed Sol's in-memory process-output limit.
  cmd = PipelineSpec [P.spec argv] (PipeBytes (Bytes.empty Unit)) (PipeFileNew source) PipeOutputInherit limit PipeFail;
  _ = case Proc.pipeline cmd of Ok (PipelineResult 0 _ _ _) -> Unit | other -> error "FEA case {params} failed: {other}";
  spec = P.spec (host + ["measure",family,source,target] + params);
  result = unwrap (J.parse (unwrap (P.output (Proc.query spec))));
  passed =
    if family == "hex20" then
      _ = print (unwrap (J.text (unwrap (J.get "line" result))));
      unwrap (J.num (unwrap (J.get "worst" result))) <= 0.000001
    else unwrap (J.get "passed" result) == J.JBool True;
  (target,passed).

batch exe python family cells =
  scripts = (case unwrap (Env.get "COSCAD_FEA_SCRIPTS") of EnvText dir -> dir | EnvMissing -> "scripts");
  host = [python,"{scripts}/fea_reference_host.py"];
  tmp = tempDir "coscad-fea-reference";
  records = List.map (runCase exe host family tmp) (B.indexed (fixtures family cells));
  passed = List.all (fn r -> (file,ok) = r; ok) records;
  outputs = List.map (fn r -> (file,ok) = r; file) records;
  spec = P.spec (host + ["summary",family,if passed then "pass" else "fail"] + outputs);
  _ = case Proc.streamNow spec of Ok 0 -> Unit | other -> error "could not print comparison: {other}";
  if passed then Unit else
    _ = if family == "hex20" then unwrap (IO.stderrText "Hex20 differs from the independent reference\n") else Unit;
    Script.exit 1.

> usage = "usage: fea-reference.sol beam|cad|hex20 --exe PATH [--python PATH] [HEX20-CELLS ...]";
  argv = args Unit;
  if List.has "--help" argv or List.has "-h" argv then print usage else
    opts = unwrap (C.parse [C.Option "exe" "" C.One,C.Option "python" "" C.One] argv);
    exe = C.valueOr "" "exe" opts;
    case C.operands opts of family :: cells ->
      if exe == "" then error usage else batch exe (C.valueOr "python3" "python" opts) family cells
    | [] -> error usage.
