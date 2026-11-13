# Refresh the source archive URL and SHA-256 in the Homebrew formula.
D = use "sol/lib/digest".
P = use "sol/lib/proc".
oneField key lines =
  matches = List.filter (fn line -> or (Str.startsWith "  {key} '" line) (Str.startsWith "  {key} \"" line)) lines;
  case matches of [line] -> line | _ -> error "expected one quoted {key} in the formula".
update version =
  _ = if version == "" then error "expected a release version" else Unit;
  # Reject characters that could break the formula's single-quoted Ruby URL.
  _ = if List.all (fn c -> or (and (c >= 48) (c <= 57))
         (or (and (c >= 65) (c <= 90)) (or (and (c >= 97) (c <= 122)) (List.has c [45,46,95,43])))) (Str.codes version)
      then Unit else error "invalid release version";
  root = Str.trim (unwrap (P.output (Proc.query (P.spec ["git","rev-parse","--show-toplevel"]))));
  formula = "{root}/Formula/coscad.rb";
  contents = readPath formula;
  lines = Str.lines contents;
  urlLine = oneField "url" lines;
  shaLine = oneField "sha256" lines;
  url = "https://github.com/hyperswine/coscad/archive/refs/tags/v{version}.tar.gz";
  file = tempFile "coscad-formula";
  _ = case Proc.streamNow (P.spec ["curl","-fsSL",url,"-o",file]) of
        Ok 0 -> Unit | Ok code -> error "download exited {code}" | Err why -> error why;
  sha = unwrap (D.sha256File file);
  replaceLine line = if line == urlLine then "  url '{url}'" else if line == shaLine then "  sha256 '{sha}'" else line;
  ending = if Str.endsWith "\n" contents then "\n" else "";
  _ = writePath formula (Str.join "\n" (List.map replaceLine lines) + ending);
  print "Formula/coscad.rb -> v{version} ({sha})".
> case args Unit of [version] -> update version | _ -> error "usage: update-formula.sol VERSION".
