# Assemble the documentation pages, man page and the rendered assembly guides.
# Build pages are cached per build by a hash of its inputs and the coscad binary,
# so an update replans only the builds that changed.

P = use "sol/lib/proc".
B = use "sol/lib/base".
L = use "std/list".
O = use "std/order".

pages = [("README","Overview"),("docs/ORBIT-TOWER","Orbit Tower notes"),("docs/LANGEXTENSION","Language"),
         ("docs/TOPOLOGICAL","Topological"),("docs/MANUFACTURING","Manufacturing"),("docs/PLAN","Build plans"),
         ("docs/LESSONS","Lessons"),("docs/SYNTAX","Syntax modes"),("docs/EXAMPLES","Examples"),
         ("docs/TESSERACT","Tesseract"),("CHANGELOG","Changelog")].

plans = ["plan/corner_pair","corner/corner","plan/cube","bow3/bow3","ball/ball","tesseract/tesseract","orbit-tower/orbit-tower","tank/tank","spiral/spiral-launcher-build","sg90/sg90-rack","gearbox/tt-m5-gearbox","handcrank/hand-crank"].

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

assemblies site scratch sol cache =
  _ = List.map (fn name -> command ["cp","-R","examples/assemble/{name}","{scratch}/{name}"]) ["plan","bow3","ball"];
  _ = copyShapes "models/tesseract-v3-threaded" "{scratch}/tesseract" "";
  _ = command [sol,"models/tesseract-v3-threaded/instructions.sol","models/tesseract-v3-threaded/tesseract.assemble","{scratch}/tesseract/tesseract.assemble"];
  _ = copyShapes "examples/assemble" "{scratch}/corner" "corner";
  _ = copyShapes "models/orbit-tower" "{scratch}/orbit-tower" "orbit-tower";
  _ = copyShapes "models/tank-model-2020" "{scratch}/tank" "tank";
  _ = copyShapes "models/spiral-launcher" "{scratch}/spiral" "spiral-launcher-build";
  _ = copyShapes "models/sg90-rack/press-fit" "{scratch}/sg90" "sg90-rack";
  # The hand-crank parts carry the gearbox definitions, so they plan on their own.
  _ = copyShapes "models/tt-propeller-gearbox/m5/hand-crank" "{scratch}/handcrank" "hand-crank";
  # The print plate and assembly share the same named gearbox solids.
  gearbox = "models/tt-propeller-gearbox/m5";
  gearDir = "{scratch}/gearbox";
  _ = command ["mkdir","-p",gearDir];
  definitions = output ["sed","/^main =/d","{gearbox}/gearbox.coscad"];
  _ = List.map (fn part -> put "{gearDir}/{part}.coscad" (definitions + "\nmain = {part}\n")) ["base","driver","idler","output","cover"];
  _ = command ["cp","docs/evidence/2026-11-24-assembly-planner/train.assemble","{gearDir}/tt-m5-gearbox.assemble"];
  # the launcher's feet are the tank model's 100 mm rails, referenced as ../tank-model-2020/rail100.coscad
  _ = command ["mkdir","-p","{scratch}/tank-model-2020"];
  _ = command ["cp","models/tank-model-2020/rail100.coscad","{scratch}/tank-model-2020/"];
  _ = command ["find",scratch,"-name","*.png","-delete"];
  _ = command ["mkdir","-p","{site}/builds",cache];
  # Fingerprint every build before planning any: builds that share an input
  # folder (plan/) write their outputs into it.
  compiler = line ["sh","-c","shasum -a 256 \"$(command -v coscad)\" | cut -c1-64"];
  shared = folderHash "{scratch}/tank-model-2020";
  keyed = List.map (fn part -> (part, line ["sh","-c","printf '%s\\n' \"$@\" | shasum -a 256 | cut -c1-16","key",
      compiler, shared, part, folderHash "{scratch}/{folderOf part}"])) plans;
  _ = List.map (fn pk -> (part, key) = pk; siteBuild site scratch cache part key) keyed;
  gearPage = "{site}/builds/tt-m5-gearbox/index.html";
  notes = output ["pandoc","-f","gfm","-t","html5","{gearbox}/README.md"];
  intro = "<section><p>The illustrated steps cover the housing and five gears. See the assembly notes for the motor mount, propeller collar, spacers and eight M5 fasteners.</p><details><summary>Full assembly notes and hardware</summary>{notes}</details></section>";
  _ = put gearPage (readPath gearPage |> Str.replace "<main>" ("<main>" + intro));
  crankPage = "{site}/builds/hand-crank/index.html";
  crankNotes = output ["pandoc","-f","gfm","-t","html5","models/tt-propeller-gearbox/m5/hand-crank/README.md"];
  crankIntro = "<section><p>Converts the M5 gearbox to a hand crank. The base and input gear are the existing gearbox parts; the stand, cover, crank, sleeve and knob are new.</p><details><summary>Full notes, checks and print record</summary>{crankNotes}</details></section>";
  _ = put crankPage (readPath crankPage |> Str.replace "<main>" ("<main>" + crankIntro));
  _ = List.map (fn part -> name = B.baseName part;
    command ["test","-s","{site}/builds/{name}/{name}_plan.json"]) plans;
  _ = command (["coscad","site","--index","{site}/builds"] + List.map B.baseName plans);
  command ["test","-s","{site}/builds/index.html"].

# "tank/tank" -> "tank": plans are listed as FOLDER/NAME.
folderOf part = Str.slice part 0 (Str.find 47 part).

# Content hash of a folder's files (names and bytes), short form.
folderHash dir = line ["sh","-c","cd \"$1\" && find . -type f -print0 | LC_ALL=C sort -z | xargs -0 shasum -a 256 | shasum -a 256 | cut -c1-16","hash",dir].

# One build's pages: reuse the cached copy when its inputs and the compiler are
# unchanged, otherwise plan it alone and replace its cache entry.
siteBuild site scratch cache part key =
  name = B.baseName part;
  cached = "{cache}/{name}/{key}";
  if P.succeeded (Proc.query (P.spec ["test","-s","{cached}/build.json"])) then
    _ = print "{name}: unchanged, reusing {key}";
    command ["cp","-R",cached,"{site}/builds/{name}"]
  else
    out = tempDir "coscad-manual-build";
    _ = command ["coscad","site",out,"{scratch}/{part}.assemble"];
    _ = command ["rm","-rf","{cache}/{name}"];
    _ = command ["mkdir","-p","{cache}/{name}"];
    _ = command ["cp","-R","{out}/{name}",cached];
    _ = command ["rm","-rf",out];
    command ["cp","-R",cached,"{site}/builds/{name}"].

# Swap the finished site in: stage a copy beside the output (same file system), then two renames.
publish site outputDir =
  staged = "{outputDir}.tmp";
  old = "{outputDir}.old";
  _ = command ["rm","-rf",staged,old];
  _ = command ["cp","-R",site,staged];
  _ = command ["chmod","-R","a+rX",staged];
  _ = if P.succeeded (Proc.query (P.spec ["test","-e",outputDir])) then command ["mv",outputDir,old] else Unit;
  _ = command ["mv",staged,outputDir];
  command ["rm","-rf",old].

build outputDir =
  sol = case unwrap (Env.get "SOL") of EnvText path -> (if path == "" then "sol" else path) | EnvMissing -> "sol";
  revision = gitStamp Unit;
  stamp = line ["date","-u","+%Y-%m-%d %H:%MZ"];
  # Planned build pages are cached by content; MANUAL_CACHE overrides the folder.
  cache = case unwrap (Env.get "MANUAL_CACHE") of EnvText path -> (if path == "" then "{outputDir}-cache" else path) | EnvMissing -> "{outputDir}-cache";
  site = tempDir "coscad-manual-site";
  scratch = tempDir "coscad-manual-input";
  _ = documents site scratch revision stamp;
  _ = assemblies site scratch sol cache;
  # Run the finishing transaction to completion before copying its output.
  _ = command [sol,"scripts/finish-manual.sol",site];
  _ = put "{site}/version.json" ("\{\"commit\": \"{revision}\", \"built\": \"{stamp}\"\}\n");
  # PipePublish creates private files. Apply the caller's umask for public pages.
  _ = command ["chmod","=rw","{site}/version.json","{site}/man.html"];
  _ = publish site outputDir;
  _ = command ["rm","-rf",site,scratch];
  print "manual built at {outputDir} ({revision})".

> case args Unit of [out] -> build out | _ -> error "usage: build-manual.sol OUTDIR".
