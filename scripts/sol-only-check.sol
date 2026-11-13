# Reject repository-owned Python/shell files, including extensionless launchers.
present flag path = ProcessResult code _ _ = unwrap (Proc.runNow (ProcessSpec ["test",flag,path] "" [] "" 0)); code == 0.
query argv = ProcessResult code out err = unwrap (Proc.runNow (ProcessSpec argv "" [] "" 0));
  if code == 0 or code == 1 then out else error "policy query failed: {err}".
legacy path = Str.endsWith ".py" path or Str.endsWith ".sh" path.
script path = if not (present "-f" path) then False else
  first = case Str.lines (readPath path) of line :: _ -> line | [] -> "";
  Str.startsWith "#!" first and List.any (fn word -> Str.contains word first) ["/sh","bash","python"].
main : unsafe Unit -> Unit .
main Unit =
  tracked = List.filter (fn p -> p != "" and present "-f" p) (Str.lines (query ["git","ls-files","--cached","--others","--exclude-standard","--","*.py","*.sh"]));
  candidates = List.filter (fn p -> p != "") (Str.lines (query ["git","grep","--untracked","-I","-l","-E","^#!.*(bash|/sh|python)","--"]));
  bad = List.append (List.filter legacy tracked) (List.filter script candidates);
  if bad == [] then print "Sol-only script policy: passed" else error "retired script files: {bad}".
> main Unit.
