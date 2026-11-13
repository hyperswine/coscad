# Check the CLI usage, manual, README and source module map for drift.
P = use "sol/lib/proc".
L = use "std/list".
O = use "std/order".
query argv = unwrap (P.output (Proc.query (P.spec argv))).
matches spec =
  case Proc.query spec of
    Ok (ProcessResult 0 out _) -> Str.lines out
  | Ok (ProcessResult 1 _ _) -> []
  | Ok (ProcessResult code _ err) -> error "grep exited {code}: {err}"
  | Err why -> error why.
findText pattern input = matches (P.withStdin input (P.spec ["grep","-Eo",pattern])).
section source start end =
  i = Str.indexOf start source;
  if i < 0 then "" else
    tail = Str.slice source (i + Str.len start) (Str.len source);
    j = Str.indexOf end tail;
    start + (if j < 0 then tail else Str.slice tail 0 j).
names prefix pattern input =
  findText pattern input |> List.map (fn match -> Str.slice match (Str.len prefix) (Str.len match)).
whenMissing message present = if present then [] else [message].
commandErrors cmds synopsis readme =
  noCommands = whenMissing "app/Main.hs: no commands found in usage" (cmds != []);
  missing = List.concat (List.map (fn cmd ->
    whenMissing "man/coscad.1: SYNOPSIS does not list `{cmd}` (in --help)"
      (findText ("^\\.Cm " + cmd + "([^[:alnum:]_]|$)") synopsis != []) +
    whenMissing "README.md: never shows `coscad {cmd}` (in --help)"
      (findText ("coscad " + cmd + "([^[:alnum:]_]|$)") readme != [])) cmds);
  extra = names ".Cm " "^\\.Cm [a-z]+" synopsis |> List.filter (fn cmd -> not (List.has cmd cmds));
  noCommands + missing + List.map (fn cmd -> "man/coscad.1: SYNOPSIS lists `{cmd}`, which --help does not") extra.
exitCases = [("validate/plan design errors",["design error"]),("check overlaps",["overlap"]),
      ("next --strict printability warnings",["--strict","Fl -strict"]),("a part that does not fit the plate",["plate"]),
      ("constraint loops that do not close",["loop"]),("parts no fixed root reaches",["fixed root"]),
      ("an instance fixed twice",["fixed twice"])].
statusErrors label s =
  if s == "" then ["{label}: exit-status text not found"] else
    flat = s |> Str.replace "\"" " " |> Str.replace "," " " |> Str.replace "\n" " " |> Str.replace "\r" " " |> Str.replace "\t" " " |> Str.words |> Str.join " ";
    codes = List.concat (List.map (fn code -> whenMissing "{label}: does not mention exit {code}" (Str.contains code flat)) ["1","2"]);
    codes + List.concat (List.map (fn pair -> (cause,words) = pair;
      whenMissing "{label}: exit 2 case missing: {cause}" (List.any (fn w -> Str.contains w flat) words)) exitCases).

stalePatterns = [("[Ee]xit(s| code)? 1 (iff|if|when) any (OVERLAP|overlap)","check exits 2 on an overlap, not 1"),
    ("\\.assemble` files always resolve in legacy mode",".assemble files accept the !simple/!glyph pragma")].
staleIn path = List.concat (List.map (fn pair -> (pattern,why) = pair;
  lines = matches (P.spec ["grep","-nEo",pattern,path]);
  List.map (fn line -> number = Str.slice line 0 (Str.find 58 line); "{path}:{number}: stale text ({why})") lines) stalePatterns).
files argv = Str.split 0 (query argv) |> List.filter (fn name -> name != "") |> L.sortWith O.string.
check u =
  mainHs = readPath "app/Main.hs";
  readme = readPath "README.md";
  man = readPath "man/coscad.1";
  usage = section mainHs "usage h =" "\n\n";
  cmds = names "\"  coscad " "\"  coscad [a-z]+" usage |> L.unique |> L.sortWith O.string;
  synopsis = section man ".Sh SYNOPSIS" ".Sh ";
  usageErrors = commandErrors cmds synopsis readme;
  exits = statusErrors "coscad --help (app/Main.hs)" (section mainHs "Exit status:" "Diagnostics go") +
          statusErrors "man/coscad.1 EXIT STATUS" (section man ".Sh EXIT STATUS" "\n.Sh ") +
          statusErrors "README.md exit status" (section readme "exit 1\nmeans" "\n\n");
  docs = ["README.md"] + files ["find","docs","-maxdepth","1","-name","*.md","-print0"] + ["man/coscad.1"];
  stale = List.concat (List.map staleIn docs);
  moduleMap = section readme "## Module map" "\n## ";
  modules = files ["find","src/Coscad","-name","*.hs","-print0"];
  absent = List.filter (fn path -> not (Str.contains (Str.slice path 11 (Str.len path)) moduleMap)) modules;
  errors = usageErrors + exits + stale + List.map (fn path -> "README.md: module map does not list {path}") absent;
  if errors == [] then print "docs agree: {List.len cmds} commands, exit status in 3 places, module map complete"
  else _ = unwrap (IO.stderr (unwrap (Utf8.encode (Str.join "\n" (List.map (fn err -> "docs: {err}") errors) + "\n")))); Script.exit 1.
> check Unit.
