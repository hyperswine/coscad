# Poll origin/main; when it moved (or with --force), rebuild the compiler and the
# manual site. Run from the checkout by the launchd job on the machine that serves
# manual.cswine.cloud (deploy/cloud.cswine.manual-update.plist):
#   sol scripts/manual-update.sol [--force]
# WWW and BIN override ~/srv/manual-www and ~/srv/bin. One update runs at a time: the
# job holds .update-lock in the checkout and does the work in a child process, so the
# lock is released even when the update fails.
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
envOr name fallback = case unwrap (Env.get name) of EnvText v -> (if v == "" then fallback else v) | EnvMissing -> fallback.
sol u = envOr "SOL" "sol".

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
    _ = command [sol Unit,"scripts/build-manual.sol",www];
    print "{stamp Unit} done".

poll flag =
  home = envOr "HOME" ".";
  www = envOr "WWW" "{home}/srv/manual-www";
  bin = envOr "BIN" "{home}/srv/bin";
  if P.succeeded (Proc.query (P.spec ["mkdir",".update-lock"])) then
    result = Proc.streamNow (P.spec [sol Unit,"scripts/manual-update.sol","--worker",www,bin,flag]);
    _ = command ["rmdir",".update-lock"];
    case result of Ok 0 -> Unit | Ok code -> error "update exited {code}" | Err why -> error why
  else print "{stamp Unit} another update is running".

> case args Unit of
    ["--worker",www,bin,flag] -> update www bin flag
  | [] -> poll ""
  | ["--force"] -> poll "--force"
  | _ -> error "usage: sol scripts/manual-update.sol [--force]".
