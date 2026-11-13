# Expand the assembly's existing mate declarations into planner instructions.
hubPath src = case List.rev (Str.split 47 src) of
  name :: dirs -> Str.join "/" (List.rev ("hub.coscad" :: dirs))
  | [] -> "hub.coscad".
pitchOf hub = case List.filter (Str.startsWith "pitch = ") (Str.lines hub) of
  first :: rest -> (case Str.words first of
    key :: eq :: pitch :: tail -> pitch | _ -> error "missing hub pitch value")
  | [] -> error "missing pitch = declaration in hub.coscad".
joint pitch line = case Str.words line of
    "mate" :: child :: socket :: parent :: anchor :: tail ->
      if and (Str.startsWith "@" socket) (Str.startsWith "@" anchor) then
        (if Str.startsWith "bar" child then ["joint drop {child} {parent}"]
         else ["joint screw {child} {parent} pitch={pitch}"])
      else error "malformed mate: {line}"
  | _ -> error "malformed mate: {line}".
stripNewlines : (lines : List String | measure lines) -> List String.
stripNewlines lines = case lines of "" :: rest -> stripNewlines rest | _ -> lines.
instructions src dest =
  source = readPath src |> Str.replace "\r\n" "\n" |> Str.replace "\r" "\n";
  pitch = pitchOf (readPath (hubPath src));
  lines = Str.split 10 source |> List.rev |> stripNewlines |> List.rev;
  joints = List.filter (Str.startsWith "mate ") lines |> List.map (joint pitch) |> List.concat;
  # Keep the generated header identical to the existing manual build artifact.
  header = "// Generated from tesseract.assemble by instructions.sol; edit that file.";
  allLines = List.concat [[header],lines,["","// Build instructions only: how each joint is made."],joints,[""]];
  writePath dest (Str.join "\n" allLines).
> case args Unit of [src,dest] -> instructions src dest | _ -> error "usage: instructions.sol INPUT.assemble OUTPUT.assemble".
