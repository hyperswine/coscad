# Assemble the documentation pages, man page and eight rendered assembly guides.

P = use "sol/lib/proc".
B = use "sol/lib/base".
L = use "std/list".
O = use "std/order".

pages = [("README","Overview"),("docs/ORBIT-TOWER","Orbit Tower notes"),("docs/LANGEXTENSION","Language"),
         ("docs/TOPOLOGICAL","Topological"),("docs/MANUFACTURING","Manufacturing"),("docs/PLAN","Build plans"),
         ("docs/LESSONS","Lessons"),("docs/SYNTAX","Syntax modes"),("docs/EXAMPLES","Examples"),
         ("docs/TESSERACT","Tesseract"),("CHANGELOG","Changelog")].

plans = ["plan/corner_pair","corner/corner","plan/cube","bow3/bow3","ball/ball","tesseract/tesseract","orbit-tower/orbit-tower","tank/tank"].

output argv = unwrap (P.output (Proc.query (P.spec argv))).

line argv = Str.trim (output argv).

command argv = case Proc.streamNow (P.spec argv) of
    Ok 0 -> Unit | Ok code -> error "{argv} exited {code}" | Err why -> error why.

put path contents =
  pipe = PipelineSpec [P.spec ["cat"]] (PipeBytes (unwrap (Utf8.encode contents))) (PipePublish path True) PipeOutputInherit 0 PipeFail;
  case Proc.pipeline pipe of Ok (PipelineResult 0 _ _ _) -> Unit | other -> error "cannot write {path}: {other}".

gitStamp u =
  rev = case P.output (Proc.query (P.spec ["git","rev-parse","--short","HEAD"])) of Ok s -> Str.trim s | Err why -> "dev";
  clean = and (P.succeeded (Proc.query (P.spec ["git","diff","--quiet"])))
              (P.succeeded (Proc.query (P.spec ["git","diff","--cached","--quiet"])));
  rev + (if clean then "" else "-dirty").
pageName key = if key == "README" then "index" else Str.lower (B.baseName key).

sourceOf key =
  if exists "{key}.md" then "{key}.md" else
    suffix = "-" + B.baseName key + ".md";
    names = ls "docs" |> List.filter (Str.endsWith suffix) |> L.sortWith O.string |> List.rev;
    case names of name :: rest -> "docs/{name}" | [] -> error "no docs page for {key}".

copyShapes source dest assembly =
  _ = command ["mkdir","-p",dest];
  shapes = ls source |> List.filter (Str.endsWith ".coscad") |> List.map (fn name -> "{source}/{name}");
  if shapes == [] then error "no CoScad parts in {source}" else
    command (["cp"] + shapes + (if assembly == "" then [] else ["{source}/{assembly}.assemble"]) + [dest]).

documents site scratch revision stamp =
  nav = "<nav class=\"top\"><a class=\"brand\" href=\"index.html\">CoScad manual</a>\n" +
        "<a href=\"builds/orbit-tower/index.html\">Orbit Tower instructions</a>\n" +
        Str.join "\n" (List.map (fn page -> (key,title) = page; "<a href=\"{pageName key}.html\">{title}</a>") pages) +
        "\n<a href=\"man.html\">man coscad</a><a href=\"builds/index.html\">Builds</a></nav>\n";
  foot = "<footer>coscad {revision} · built {stamp} · <a href=\"https://github.com/hyperswine/coscad\">source</a></footer>\n";
  _ = put "{scratch}/nav.html" nav;
  _ = put "{scratch}/foot.html" foot;
  _ = command ["mkdir","-p","{site}/assets/orbit-tower"];
  _ = command ["cp","deploy/manual.css","{site}/manual.css"];
  _ = command ["cp","models/orbit-tower/assembled.png","models/orbit-tower/bed.png","{site}/assets/orbit-tower"];
  _ = List.map (fn page -> (key,title) = page;
    command ["pandoc","-s","-f","gfm","-t","html5","--css","manual.css","--metadata","title={title} · CoScad",
             "-B","{scratch}/nav.html","-A","{scratch}/foot.html",sourceOf key,"-o","{site}/{pageName key}.html"]) pages;
  man = output ["mandoc","-T","html","-O","fragment","man/coscad.1"];
  head = "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"><title>man coscad</title><link rel=\"stylesheet\" href=\"manual.css\"></head><body>\n";
  put "{site}/man.html" (head + nav + man + foot + "</body></html>\n").

assemblies site scratch sol =
  _ = List.map (fn name -> command ["cp","-R","examples/assemble/{name}","{scratch}/{name}"]) ["plan","bow3","ball"];
  _ = copyShapes "models/tesseract-v3-threaded" "{scratch}/tesseract" "";
  _ = command [sol,"models/tesseract-v3-threaded/instructions.sol","models/tesseract-v3-threaded/tesseract.assemble","{scratch}/tesseract/tesseract.assemble"];
  _ = copyShapes "examples/assemble" "{scratch}/corner" "corner";
  _ = copyShapes "models/orbit-tower" "{scratch}/orbit-tower" "orbit-tower";
  _ = copyShapes "models/tank-model-2020" "{scratch}/tank" "tank";
  _ = command ["find",scratch,"-name","*.png","-delete"];
  _ = command (["coscad","site","{site}/builds"] + List.map (fn part -> "{scratch}/{part}.assemble") plans);
  _ = List.map (fn part -> name = B.baseName part;
    command ["test","-s","{site}/builds/{name}/{name}_plan.json"]) plans;
  command ["test","-s","{site}/builds/index.html"].

build outputDir =
  sol = case unwrap (Env.get "SOL") of EnvText path -> (if path == "" then "sol" else path) | EnvMissing -> "sol";
  revision = gitStamp Unit;
  stamp = line ["date","-u","+%Y-%m-%d %H:%MZ"];
  site = tempDir "coscad-manual-site";
  scratch = tempDir "coscad-manual-input";
  _ = documents site scratch revision stamp;
  _ = assemblies site scratch sol;
  # Run the finishing transaction to completion before copying its output.
  _ = command [sol,"scripts/finish-manual.sol",site];
  _ = put "{site}/version.json" ("\{\"commit\": \"{revision}\", \"built\": \"{stamp}\"\}\n");
  # PipePublish creates private files. Apply the caller's umask for public pages.
  _ = command ["chmod","=rw","{site}/version.json","{site}/man.html"];
  _ = command ["python3","scripts/publish-manual.py",site,outputDir];
  print "manual built at {outputDir} ({revision})".

> case args Unit of [out] -> build out | _ -> error "usage: build-manual.sol OUTDIR".
