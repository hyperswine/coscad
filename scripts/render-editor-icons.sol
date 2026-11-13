# Compile the catalog shapes and render the editor's SVG symbols as one batch.
C = use "sol/lib/cli".
J = use "sol/lib/json".
P = use "sol/lib/proc".
text key item = unwrap (J.text (unwrap (J.get key item))).
stage compiler tmp item =
  name = text "id" item;
  _ = if name == "" or List.has name [".",".."] or Str.contains "/" name then error "invalid shape ID" else Unit;
  file = "{tmp}/{name}.coscad";
  content = "main = " + text "source" item + "\n";
  put = PipelineSpec [P.spec ["cat"]] (PipeBytes (unwrap (Utf8.encode content))) (PipeFileNew file) PipeOutputInherit 0 PipeFail;
  _ = case Proc.pipeline put of Ok (PipelineResult 0 _ _ _) -> Unit | other -> error "could not stage {name}: {other}";
  _ = unwrap (P.output (Proc.query (P.spec [compiler,"stl",file,"-o","{tmp}/{name}.stl"])));
  name.
render root opts compiler =
  catalog = C.valueOr "{root}/vscode/shape-icons.json" "catalog" opts;
  destination = C.valueOr "{root}/vscode/assets/shapes" "output" opts;
  rows = unwrap (J.items (unwrap (J.parse (readPath catalog))));
  tmp = tempDir "coscad-editor-icons";
  names = List.map (stage compiler tmp) rows;
  python = C.valueOr "python3" "python" opts;
  draw = P.withStdin (J.render (J.JArr rows)) (P.spec [python,"{root}/scripts/render-editor-icons.py","--render-meshes",tmp]);
  _ = case Proc.streamNow draw of Ok 0 -> Unit | other -> error "SVG render failed: {other}";
  _ = unwrap (P.output (Proc.query (P.spec ["mkdir","-p",destination])));
  _ = List.map (fn name -> writePath "{destination}/{name}.svg" (readPath "{tmp}/{name}.svg")) names;
  print "Rendered {List.len names} bundled shape symbols".
> usage = "usage: render-editor-icons.sol --coscad PATH [--python PATH] [--catalog FILE] [--output DIR]";
  argv = args Unit;
  if List.has "--help" argv or List.has "-h" argv then print usage else
    opts = unwrap (C.parse [C.Option "coscad" "" C.One,C.Option "python" "" C.One,C.Option "catalog" "" C.One,C.Option "output" "" C.One] argv);
    exe = C.valueOr "" "coscad" opts;
    root = case unwrap (Env.get "COSCAD_ICON_ROOT") of EnvText path -> path | EnvMissing -> ".";
    if C.operands opts == [] and exe != "" then render root opts exe else error usage.
