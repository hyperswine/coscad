# Select the master design's main definition for each printable part.
parts = [("hub-blockers","hub_blockers"), ("inner-blockers","inner_blockers"),
         ("bar-print","bar_print"), ("hub","hub"), ("cap","cap_part"),
         ("bar","bar"), ("strut","strut"), ("inner","inner"),
         ("coupon-socket","coupon_socket"), ("coupon-screw","coupon_screw"),
         ("male","male"), ("female-tool","female")].
exportParts dir =
  source = readPath "{dir}/design.coscad" |> Str.replace "\r\n" "\n" |> Str.replace "\r" "\n";
  lines = Str.split 10 source;
  ending = if Str.endsWith "\n" source then "\n" else "";
  isMain = Str.startsWith "main =";
  _ = if List.len (List.filter isMain lines) == 1 then Unit else error "expected one main definition in design.coscad";
  # Preserve the existing generated source bytes and their recorded hashes.
  header = "// Selected from design.coscad by export.sol; edit the master.\n";
  _ = List.map (fn part ->
    (name,expression) = part;
    selected = List.map (fn line -> if isMain line then "main = {expression}" else line) lines;
    writePath "{dir}/{name}.coscad" (header + Str.join "\n" selected + ending)) parts;
  print "Selected {List.len parts} pure CoScad definitions".
> case args Unit of
    [] -> exportParts (if exists "models/tesseract-v3-threaded/design.coscad" then "models/tesseract-v3-threaded" else ".")
  | [dir] -> exportParts dir
  | _ -> error "usage: export.sol [TESSERACT-DIR]".
