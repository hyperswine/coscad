# Batch native meshing for the three boat models, with a combined log per part.
B = use "sol/lib/base".
C = use "sol/lib/cli".
P = use "sol/lib/proc".
query spec = Str.trim (unwrap (P.output (Proc.query spec))).
groups model = case model of
    "beach-monohull" -> [["beach-monohull"],["beach-monohull-envelope"],["beach-monohull-sealed"]]
  | "beach-outrigger" -> [["beach-outrigger","beach-outrigger-envelope"]]
  | "twin-boat-trials" -> [["monohull","monohull-envelope","monohull-opening"],
                          ["outrigger","outrigger-envelope","outrigger-opening"]]
  | _ -> error "unknown boat model: {model}".
launch compiler root out name =
  argv = ["sh","-c","log=$1; shift; exec \"$@\" >\"$log\" 2>&1","render-boat",
          "{out}/{name}-render.log",compiler,"stl","{root}/{name}.coscad","-o","{out}/{name}.stl"];
  pipeline = PipelineSpec [P.spec argv] (PipeBytes (Bytes.empty Unit)) PipeOutputInherit PipeOutputInherit 0 PipeFail;
  case Proc.start pipeline of Ok job -> (name,job) | Err reason -> error (str reason).
finish (name,job) = case Proc.wait job of
    Ok (PipelineResult 0 _ _ _) -> print "Rendered {name}"
  | Ok (PipelineResult status _ _ _) -> error "{name} render exited {status}"
  | Err reason -> error (str reason).
render dir opts =
  root = query (P.spec ["realpath",dir]);
  batches = groups (B.baseName root);
  fallback = case unwrap (Env.get "COSCAD") of EnvText path -> path | EnvMissing -> "";
  selected = C.valueOr fallback "coscad" opts;
  compiler = if selected != "" then selected else query (P.inDir "{root}/../.." (P.spec ["cabal","list-bin","exe:coscad"]));
  out = C.valueOr root "out" opts;
  _ = query (P.spec ["mkdir","-p",out]);
  _ = List.map (fn names -> List.map (launch compiler root out) names |> List.map finish) batches;
  Unit.
> opts = unwrap (C.parse [C.Option "coscad" "" C.One,C.Option "out" "" C.One] (args Unit));
  case C.operands opts of [dir] -> render dir opts | _ -> error "usage: render-boats.sol [--coscad PATH] [--out DIR] MODEL-DIR".
