# Locate a model directory from the working directory without side effects.
#
# In Sol 0.13.1 `exists`, Try.exists, Try.stat and Try.readPath on a missing
# path create its missing parent directories, so the usual
# `if exists "models/X/script.sol"` probe litters the tree with empty folders
# when run from elsewhere. Try.ls on a missing directory has no such effect.
# The model directory itself is not listed either: a listing is a read claim,
# and scripts that publish files immediately (Fs.copyPublishNow) into it would
# then conflict with their own transaction at commit. So: list the parent,
# then test the marker file only once its directory is known to exist.
#
#   R = use "../lib/root".
#   dir = R.modelDir "models/pla-bow-launcher" "build.sol".
#
# Accepts the repository root or the model directory itself as the cwd.
lastSlash s = List.fold (fn acc i -> if Str.at s i == 47 then i else acc) (0 - 1) (List.range 0 (Str.len s - 1)).
modelDir rel marker =
  cwd = unwrap (IO.cwd Unit);
  root = if Str.endsWith ("/" + rel) cwd then cwd else "{cwd}/{rel}";
  cut = lastSlash root;
  parent = Str.slice root 0 cut;
  base = Str.slice root (cut + 1) (Str.len root);
  usage = "run from the CoScad root or {rel}";
  case Try.ls parent of
    Ok names -> if and (List.has base names) (exists "{root}/{marker}") then root else error usage
  | Err e -> error usage.
