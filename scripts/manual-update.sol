# The manual-update.sh entry point holds the checkout lock throughout this worker.
P = use "sol/lib/proc".
output argv = Str.trim (unwrap (P.output (Proc.query (P.spec argv)))).
command argv = case Proc.streamNow (P.spec argv) of
    Ok 0 -> Unit | Ok code -> error "{argv} exited {code}" | Err why -> error why.
tailLog lines argv =
  combined = P.spec (["sh","-c","exec \"$@\" 2>&1","manual-update"] + argv);
  pipe = PipelineSpec [combined,P.spec ["tail","-n",lines]] (PipeBytes (Bytes.empty Unit)) PipeOutputInherit PipeOutputInherit 0 PipeFail;
  case Proc.pipeline pipe of
    Ok (PipelineResult 0 _ _ _) -> Unit
  | Ok (PipelineResult code _ _ _) -> error "{argv} exited {code}"
  | Err why -> error (str why).
stamp u = output ["date","-u","+%FT%TZ"].
update www bin flag =
  _ = command ["git","fetch","-q","origin","main"];
  previous = output ["git","rev-parse","HEAD"];
  incoming = output ["git","rev-parse","origin/main"];
  current = previous == incoming and flag != "--force" and P.succeeded (Proc.query (P.spec ["test","-f","{www}/index.html"]));
  if current then Unit else
    _ = print "{stamp Unit} updating {Str.slice previous 0 7} -> {Str.slice incoming 0 7}";
    _ = command ["git","reset","-q","--hard","origin/main"];
    _ = command ["mkdir","-p",bin];
    _ = tailLog "1" ["cabal","update"];
    _ = tailLog "3" ["cabal","install","exe:coscad","--install-method=copy","--overwrite-policy=always","--installdir={bin}"];
    _ = command ["coscad","doctor"];
    _ = command ["scripts/build-manual.sh",www];
    print "{stamp Unit} done".
> case args Unit of [www,bin,flag] -> update www bin flag | _ -> error "use scripts/manual-update.sh [--force]".
