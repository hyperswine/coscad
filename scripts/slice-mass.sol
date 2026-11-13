# Report sliced mass and balance using the existing NumPy toolpath integrator.
C = use "sol/lib/cli".
D = use "sol/lib/digest".
J = use "sol/lib/json".
L = use "std/list".
P = use "sol/lib/proc".

env key fallback = case unwrap (Env.get key) of EnvMissing -> fallback | EnvText s -> if s == "" then fallback else s.
fail why = _ = unwrap (IO.stderrText ("error: {why}\n")); Script.exit 1.
must result = case result of Ok value -> value | Err why -> _ = fail why; error why.
usage = "usage: slice-mass.sol MANIFEST [-o REPORT] [--components-out COMPONENTS] [--python PYTHON]".
strings j = must (J.items j) |> List.map (fn value -> must (J.text value)).
runChild spec = case Proc.streamNow spec of
  Ok 0 -> Unit | Ok code -> _ = fail "mass reader exited {code}"; Unit | Err why -> fail why.
publish host source target = runChild (P.spec (host + ["publish",source,target])).
mass opts input =
  python = C.valueOr (env "COSCAD_MASS_PYTHON" "python3") "python" opts;
  scripts = env "COSCAD_MASS_SCRIPTS" "scripts";
  host = [python,"{scripts}/slice_mass_host.py"];
  reader = "{scripts}/coscad_mass.py";
  source = must (Try.readPath input);
  # Hash the bytes the integrator receives. readPath may decode host text,
  # so compare the on-disk hash before accepting it as the receipt.
  digest = Hex.encode (Sha256.bytes (must (Utf8.encode source)));
  _ = if must (D.sha256File input) == digest then Unit else fail "manifest bytes changed during read";
  reportOut = C.values "output" opts;
  componentsOut = C.values "components-out" opts;
  raw = P.output (Proc.query (P.withStdin source (P.spec (host + ["paths",input] + reportOut + componentsOut))));
  details = must (raw |>? J.parse |>? J.items);
  case details of [name,protected,outputs] ->
    targets = strings outputs;
    inputs = strings protected;
    if List.len (L.unique targets) != List.len targets or List.any (fn path -> List.has path inputs) targets
    then fail "outputs must be distinct and must not overwrite inputs" else
      sourceName = must (J.text name);
      readerHash = must (D.sha256File reader);
      temp = tempDir "coscad-slice-mass";
      kind = if componentsOut == [] then "" else "components";
      _ = runChild (P.withStdin source (P.spec (host + ["integrate",sourceName,digest,readerHash,temp,kind])));
      _ = if must (D.sha256File reader) == readerHash then Unit else fail "mass reader changed during calculation";
      _ = case reportOut of [] -> runChild (P.spec ["cat","{temp}/report"])
                         | path :: rest -> publish host "{temp}/report" path;
      (case componentsOut of [] -> Unit | path :: rest -> publish host "{temp}/components" path)
  | _ -> fail "invalid resolved path list".
> argv = args Unit;
  if List.has "--help" argv or List.has "-h" argv then print usage else
    opts = case C.parse [C.Option "output" "o" C.One,C.Option "components-out" "" C.One,C.Option "python" "" C.One] argv of
      Ok choices -> choices | Err why -> _ = unwrap (IO.stderrText ("{why}\n{usage}\n")); _ = Script.exit 2; error why;
    case C.operands opts of [path] -> mass opts path | _ -> _ = unwrap (IO.stderrText (usage + "\n")); Script.exit 2.
