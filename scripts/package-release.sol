# Package a tested native CoScad executable together with its documentation and notices.
C = use "sol/lib/cli".
D = use "sol/lib/digest".
J = use "sol/lib/json".
P = use "sol/lib/proc".

payload = ["README.md","LICENSE","CHANGELOG.md","examples","stdlib","docs","man","third-party","native-kernel.lock.json"].
query argv = Str.trim (unwrap (P.output (Proc.query (P.spec argv)))).
command argv = case Proc.streamNow (P.spec argv) of
    Ok 0 -> Unit | Ok code -> error "exit {code}: {argv}" | Err why -> error why.
pipeline spec = case Proc.pipeline spec of
    Ok (PipelineResult 0 _ _ _) -> Unit
  | Ok (PipelineResult code _ _ _) -> error "archive operation exited {code}"
  | Err why -> error (str why).
textFile path content = pipeline (PipelineSpec [P.spec ["cat"]] (PipeBytes (unwrap (Utf8.encode content)))
                              (PipePublish path True) PipeOutputInherit 0 PipeFail).
tar dir argv = P.spec (["tar"] + argv) |> P.inDir dir |> P.withEnv "COPYFILE_DISABLE" "1".
metadata root binary system arch =
  versionLine = query [binary,"--version"];
  version = if Str.startsWith "coscad " versionLine then Str.slice versionLine 7 (Str.len versionLine) else versionLine;
  caps = unwrap (J.parse (query [binary,"capabilities","--json"]));
  _ = if and (J.get "default_backend" caps == Ok (J.JStr "native")) (J.get "backends" caps == Ok (J.strs ["native"]))
      then Unit else error "Release must be the native-only build (pinned kernel linked)";
  kernel = unwrap (J.parse (readPath "{root}/native-kernel.lock.json"));
  meta = J.JObj [("name",J.JStr "coscad"),("version",J.JStr version),("system",J.JStr system),("arch",J.JStr arch),
                 ("revision",J.JStr (query ["git","-C",root,"rev-parse","HEAD"])),
                 ("minimum_macos",if system == "macos" then J.JStr "15.0" else J.JNull),("native_kernel",kernel)];
  if system != "macos" then meta else
    lines = Str.lines (query ["otool","-L",binary]);
    deps = case lines of header :: rest -> rest | [] -> error "otool returned no executable header";
    libs = List.map (fn line -> value = Str.trim line; i = Str.indexOf " (" value;
                           if i < 0 then value else Str.slice value 0 i) deps;
    unexpected = List.filter (fn lib -> not (or (Str.startsWith "/usr/lib/" lib) (Str.startsWith "/System/Library/" lib))) libs;
    if unexpected == [] then J.set "runtime_libraries" (J.strs libs) meta
    else error "Unexpected non-system runtime libraries: {unexpected}".

packageRelease root binary out =
  system = if query ["uname","-s"] == "Darwin" then "macos" else "linux";
  machine = query ["uname","-m"];
  arch = if machine == "aarch64" then "arm64" else machine;
  name = "coscad-{system}-{arch}";
  report = metadata root binary system arch;
  work = tempDir "coscad-release";
  stage = "{work}/{name}";
  _ = command ["mkdir","-p",stage,out];
  _ = command ["cp","-p",binary,"{stage}/coscad"];
  # Dereference source links as copytree does. The pipe keeps arbitrary binary
  # data outside the VM; macOS tar must not inject AppleDouble files.
  source = tar root (["-chf","-","--exclude","dist","--exclude","__pycache__","--exclude",".DS_Store"] + payload);
  _ = pipeline (PipelineSpec [source,tar stage ["-xpf","-"]] (PipeBytes (Bytes.empty Unit)) PipeDiscard PipeOutputInherit 0 PipeFail);
  _ = if P.succeeded (Proc.query (P.spec ["test","-f","{stage}/third-party/clipper2/LICENSE"])) then Unit
      else error "third-party/clipper2/LICENSE is missing";
  _ = textFile "{stage}/release.json" (J.pretty report + "\n");
  _ = command ["chmod","=rw","{stage}/release.json"];
  archive = "{name}.tar.gz";
  tempArchive = "{work}/{archive}";
  _ = pipeline (PipelineSpec [tar work ["-czf","-",name]] (PipeBytes (Bytes.empty Unit))
                 (PipeFileNew tempArchive) PipeOutputInherit 0 PipeFail);
  digest = unwrap (D.sha256File tempArchive);
  _ = case Fs.copyPublishNow tempArchive "{out}/{archive}" 65536 True of Ok size -> Unit | Err why -> error (str why);
  _ = textFile "{out}/{archive}.sha256" ("{digest}  {archive}\n");
  _ = command ["chmod","=rw","{out}/{archive}","{out}/{archive}.sha256"];
  print "{out}/{archive}".
> opts = unwrap (C.parse [C.Option "binary" "" C.One,C.Option "output" "" C.One] (args Unit));
  binary = C.valueOr "" "binary" opts;
  _ = if or (binary == "") (C.operands opts != []) then error "usage: package-release.sol --binary PATH [--output DIR]" else Unit;
  root = query ["git","rev-parse","--show-toplevel"];
  packageRelease root (query ["realpath",binary]) (C.valueOr "{root}/dist-release" "output" opts).
