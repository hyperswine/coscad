# Shared source selection, native rendering and offline slicing for both globe builds.
P = use "sol/lib/proc".
env key fallback = case unwrap (Env.get key) of EnvMissing -> fallback | EnvText s -> if s == "" then fallback else s.
command spec = case Proc.streamNow spec of Ok 0 -> Unit | Ok code -> error "command exited {code}" | Err why -> error why.
query spec = Str.trim (unwrap (P.output (Proc.query spec))).
logged log argv = P.spec (["sh","-c","log=$1; shift; exec \"$@\" >\"$log\" 2>&1","globe-build",log] + argv).
sourcePrefix : unsafe String -> String.
sourcePrefix source =
  next = Str.indexOf "main =" source;
  if next < 0 then source else
    rest = Str.slice source (next + 6) (Str.len source);
    before = Str.slice source 0 next;
    if Str.contains "main =" rest then before + "main =" + sourcePrefix rest else before.
partSource file prefix shape =
  input = PipeBytes (unwrap (Utf8.encode (prefix + "main = {shape}\n")));
  put = PipelineSpec [P.spec ["cat"]] input (PipeFileNew file) PipeOutputInherit 0 PipeFail;
  case Proc.pipeline put of Ok (PipelineResult 0 _ _ _) -> Unit | other -> error "could not stage {file}: {other}".
prefixOf : unsafe String -> String.
prefixOf file = sourcePrefix (readPath file |> Str.replace "\r\n" "\n" |> Str.replace "\r" "\n").
startRender dir tmp compiler prefix entry =
  (name,shape) = entry;
  source = "{tmp}/{name}.coscad";
  _ = partSource source prefix shape;
  spec = logged "{dir}/{name}-render.log" [compiler,"stl",source,"-o","{tmp}/{name}.stl"];
  case Proc.start (PipelineSpec [spec] (PipeBytes (Bytes.empty Unit)) PipeOutputInherit PipeOutputInherit 0 PipeFail) of
    Ok job -> (name,job) | Err why -> error (str why).
finishRender dir tmp pair =
  (name,job) = pair;
  case Proc.wait job of
    Ok (PipelineResult 0 _ _ _) ->
      _ = command (P.spec ["cp","{tmp}/{name}.stl","{dir}/{name}.stl"]);
      print "Rendered {name}"
  | Ok (PipelineResult code _ _ _) -> error "{name} render exited {code}"
  | Err why -> error (str why).
sliceProject dir slicer entry =
  (folder,process,project,output) = entry;
  out = "{dir}/{folder}";
  _ = command (P.spec ["mkdir","-p",out]);
  command (logged "{out}/slicer.log" [slicer,"--debug","2","--load-settings",
    "{dir}/presets/machine.json;{dir}/presets/{process}","--load-filaments","{dir}/presets/filament.json",
    "--arrange","0","--orient","0","--slice","1","--export-3mf",output,"--outputdir",out,"{dir}/{project}"]).
renderParts dir tmp compiler prefix batches =
  _ = List.map (fn batch -> List.map (startRender dir tmp compiler prefix) batch |> List.map (finishRender dir tmp)) batches;
  Unit.

exportView dir tmp compiler source keep view =
  (name,shape) = view;
  input = "{tmp}/{name}.coscad";
  raw = "{tmp}/{name}.scad";
  path = "{dir}/{name}.scad";
  bytes = unwrap (Utf8.encode (source + "main = {shape}\n"));
  make = PipelineSpec [P.spec ["cat"]] (PipeBytes bytes) (PipeFileNew input) PipeOutputInherit 0 PipeFail;
  _ = case Proc.pipeline make of Ok (PipelineResult 0 _ _ _) -> Unit | other -> error "cannot stage {name}: {other}";
  compile = PipelineSpec [P.spec [compiler,input,"-o",raw]] (PipeBytes (Bytes.empty Unit)) PipeDiscard PipeOutputInherit 0 PipeFail;
  _ = case Proc.pipeline compile of Ok (PipelineResult 0 _ _ _) -> Unit | other -> error "cannot compile {name}: {other}";
  # Stream the larger SCAD files (the assembly export exceeds 11 MB).
  patch = PipelineSpec [P.spec ["sed","s/[$]fn = 50;/$fn = 96;/g"]] (PipeFile raw) (PipePublish path True) PipeOutputInherit 0 PipeFail;
  _ = case Proc.pipeline patch of Ok (PipelineResult 0 _ _ _) -> Unit | other -> error "cannot export {name}: {other}";
  _ = command (P.spec ["chmod","=rw",path]);
  if name == keep then
    _ = command (P.spec ["chmod","=rw",input]);
    command (P.spec ["cp",input,"{dir}/{keep}.coscad"])
  else Unit.
