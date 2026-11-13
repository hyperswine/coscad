# Batch offline slicing for models sharing the prepared-project directory layout.
B = use "sol/lib/base".
defaultSlicer = "/Applications/BambuStudio.app/Contents/MacOS/BambuStudio".
settings model = case model of
    "beach-monohull" -> (model, ["p1s-pla","a1-pla","x1c-pla"])
  | "beach-outrigger" -> (model, ["p1s-pla","p1s-petg","a1-pla","a1-petg"])
  | "tt-propellers" -> ("props", ["x1c-props"])
  | "two-hand-top" -> ("top", ["a1-top"])
  | "tank-model-2020" -> ("tank", ["x1c-bed1","x1c-bed2"])
  | "bolted-glider" -> ("glider", ["a1-wing-right","p1s-wing-left","x1c-fuselage","a1-tail"])
  | "twin-boat-trials" -> ("", ["monohull","outrigger"])
  | _ -> error "unknown prepared-project model: {model}".
command argv = case Proc.streamNow (ProcessSpec argv "" [] "" 0) of
    Ok 0 -> Unit
  | Ok code -> error "command exited {code}: {argv}"
  | Err message -> error message.
sliceJob slicer root stem job =
  name = if stem == "" then job else stem;
  folder = "{root}/{job}";
  out = "{folder}/slice";
  presets = "{folder}/presets";
  logPath = "{out}/slicer.log";
  _ = command ["mkdir","-p",out];
  # Redirect both streams to the same log without capturing unbounded output.
  # All paths and options cross the fixed shell fragment as positional arguments.
  _ = command ["sh","-c","log=$1; shift; exec \"$@\" >\"$log\" 2>&1","slice-prepared",
      logPath,slicer,"--debug","2","--load-settings","{presets}/machine.json;{presets}/process.json",
      "--load-filaments","{presets}/filament.json","--arrange","0","--orient","0","--slice","1",
      "--export-3mf","{name}.gcode.3mf","--outputdir",out,"{folder}/{name}-project.3mf"];
  print "Sliced {job}".
batch slicer directory jobs =
  ProcessResult code path message = unwrap (Proc.query (ProcessSpec ["realpath",directory] "" [] "" 0));
  root = if code == 0 then Str.slice path 0 (Str.len path - 1) else error message;
  (stem,defaults) = settings (B.baseName root);
  selected = case jobs of [] -> defaults | _ -> jobs;
  _ = List.map (fn job ->
    if or (List.has job ["",".",".."]) (Str.contains "/" job) then error "expected a job directory name: {job}"
    else if isDir "{root}/{job}" then sliceJob slicer root stem job else error "missing job directory: {job}") selected;
  Unit.
> case args Unit of
    "--slicer" :: slicer :: directory :: jobs -> batch slicer directory jobs
  | directory :: jobs -> batch defaultSlicer directory jobs
  | _ -> error "usage: slice-prepared.sol [--slicer PROGRAM] MODEL-DIR [JOB ...]".
