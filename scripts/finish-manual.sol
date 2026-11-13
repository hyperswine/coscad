# Shared navigation, link rewriting and render verification for the CoScad manual.
P = use "sol/lib/proc".
B = use "sol/lib/base".
J = use "sol/lib/json".
pages = [("index","Overview"),("langextension","Language"),("topological","Topological"),
         ("manufacturing","Manufacturing"),("plan","Build plans"),("lessons","Lessons"),("syntax","Syntax modes"),
         ("examples","Examples"),("tesseract","Tesseract"),("changelog","Changelog"),("man","man coscad"),("orbit-tower","Orbit Tower notes")].
navigation prefix =
  links = List.map (fn p -> (name,title) = p; "<a href=\"{prefix}{name}.html\">{title}</a>") pages;
  "<nav class=\"top\" aria-label=\"Main navigation\"><div class=\"nav-inner\"><a class=\"brand\" href=\"{prefix}index.html\">CoScad</a>" +
  "<a href=\"{prefix}builds/index.html\">Builds</a><details class=\"nav-menu\"><summary>Manual</summary><div class=\"nav-links\">" +
  Str.join "" links + "</div></details></div></nav>".
observer = "<script>(()=>\{const nav=document.querySelector('nav.top');const update=()=>document.documentElement.style.setProperty('--nav-height',nav.offsetHeight+'px');update();if(window.ResizeObserver)new ResizeObserver(update).observe(nav);\})();</script>".
xml s = s |> Str.replace "&" "&amp;" |> Str.replace "<" "&lt;" |> Str.replace ">" "&gt;" |> Str.replace "\"" "&quot;" |> Str.replace "'" "&#x27;".
suffixFrom s i = Str.slice s i (Str.len s).
relativePath path =
  absolute = Str.startsWith "/" path;
  add parts part = if List.has part ["","."] then parts else if part != ".." then part :: parts else
    case parts of [] -> (if absolute then [] else [".."]) | ".." :: others -> ".." :: parts | x :: others -> others;
  components = List.fold add [] (Str.split 47 path) |> List.rev;
  prefix = if Str.startsWith "//" path and not (Str.startsWith "///" path) then "//" else if absolute then "/" else "";
  if and (components == []) (prefix == "") then "." else prefix + Str.join "/" components.
sourceLink filename href =
  if List.any (fn prefix -> Str.startsWith prefix href) ["https:","http:","#","mailto:"] then href else
    i = Str.indexOf "#" href;
    target = if i < 0 then href else Str.slice href 0 i;
    fragment = if i < 0 then "" else suffixFrom href i;
    leaf = B.baseName target;
    doc = Str.lower (Str.slice leaf 0 (Numeric.max 0 (Str.len leaf - 3)));
    known = List.map (fn p -> (name,title) = p; name) pages;
    if and (Str.endsWith ".md" target) (List.has doc ("readme" :: known)) then
      (if doc == "readme" then "index" else doc) + ".html" + fragment
    else if or (List.any (fn ext -> Str.endsWith ext target) [".md",".coscad",".assemble",".json",".py",".hs"])
               (List.any (fn p -> Str.startsWith p target) ["docs/","../","examples/","models/","experiments/"]) then
      context = if or (Str.startsWith "/" target) (List.has filename ["index.html","changelog.html","man.html"]) then "" else "docs/";
      "https://github.com/hyperswine/coscad/blob/main/" + xml (relativePath (context + target)) + fragment
    else href.
# Replace quoted href values in document order without interpreting other HTML.
rewriteLinks : unsafe String -> String -> String.
rewriteLinks filename s =
  marker = "href=\"";
  i = Str.indexOf marker s;
  if i < 0 then s else tail = suffixFrom s (i + Str.len marker); j = Str.indexOf "\"" tail;
    if j < 0 then s else Str.slice s 0 i + marker + sourceLink filename (Str.slice tail 0 j) + "\"" +
                          rewriteLinks filename (suffixFrom tail (j + 1)).
wordCode c = or (and (c >= 48) (c <= 57)) (or (and (c >= 65) (c <= 90)) (or (and (c >= 97) (c <= 122)) (c == 95))).
navTag tag = if Str.startsWith "<nav class=top" tag then not (wordCode (Str.at tag 14))
             else Str.startsWith "<nav class=\"top\"" tag.
manualCss : unsafe String -> Bool.
manualCss value = if Str.startsWith "../" value then manualCss (suffixFrom value 3) else value == "manual.css".
cssTag : unsafe String -> Bool.
cssTag tag =
  suffix = suffixFrom tag 5;
  i = Str.indexOf "href=\"" suffix;
  if or (wordCode (Str.at tag 5)) (i < 0) then False else value = suffixFrom suffix (i + 6);
    end = Str.indexOf "\"" value; and (end >= 0) (manualCss (Str.slice value 0 end)).
# Only replace the navigation and style tags owned by the manual generators.
stripTags : unsafe String -> String -> (String -> Bool) -> String -> String.
stripTags prefix keepEnd matches s =
  i = Str.indexOf prefix s;
  if i < 0 then s else tail = suffixFrom s i; j = Str.indexOf ">" tail;
    if j < 0 then s else tag = Str.slice tail 0 (j + 1); after = suffixFrom tail (j + 1);
      k = if keepEnd == "" then 0 else Str.indexOf keepEnd after;
      skipped = and (matches tag) (k >= 0);
      head = Str.slice s 0 i + (if skipped then "" else tag);
      remaining = if skipped then suffixFrom after (k + Str.len keepEnd) else after;
      head + stripTags prefix keepEnd matches remaining.
finishPage : unsafe String -> String -> String.
finishPage relative source =
  parts = Str.split 47 relative;
  prefix = Str.join "" (List.map (fn x -> "../") (List.drop 1 parts));
  build = Str.startsWith "builds/" relative;
  css = "<link rel=\"stylesheet\" href=\"{prefix}manual.css\">";
  cleaned = source |> stripTags "<nav class=" "</nav>" navTag |> stripTags "<link" "" cssTag |> Str.replace "</head>" (css + "</head>");
  page = if build then cleaned else rewriteLinks (B.baseName relative) cleaned;
  start = Str.indexOf "<body" page;
  tail = if start < 0 then "" else suffixFrom page start;
  end = Str.indexOf ">" tail;
  content = if end < 0 then page else
    offset = start + end + 1;
    body = suffixFrom page offset;
    close = Str.indexOf "</body>" body;
    wrap = not build and close >= 0 and not (Str.contains "<main class=\"manual-content\">" page);
    inside = if wrap then "<main class=\"manual-content\">" + Str.slice body 0 close + "</main>" + suffixFrom body close else body;
    Str.slice page 0 offset + navigation prefix + inside;
  if Str.contains "new ResizeObserver" content then content else Str.replace "</body>" (observer + "</body>") content.
findFiles argv = unwrap (P.output (Proc.query (P.spec (["find"] + argv)))) |> Str.split 0 |> List.filter (fn path -> path != "").
verifyPlan path =
  plan = unwrap (J.parse (readPath path));
  steps = unwrap (J.get "steps" plan |>? J.items);
  stem = Str.slice path 0 (Str.len path - Str.len "_plan.json");
  _ = List.map (fn n -> image = "{stem}_step{n}.png";
    if and (P.succeeded (Proc.query (P.spec ["test","-f",image]))) (P.succeeded (Proc.query (P.spec ["test","-s",image])))
    then Unit else error "Missing instruction image: {image}") (List.range 1 (List.len steps));
  Unit.
finish : unsafe String -> Unit.
finish directory =
  root = Str.trim (unwrap (P.output (Proc.query (P.spec ["realpath",directory]))));
  builds = "{root}/builds";
  plans = if isDir builds then findFiles [builds,"-mindepth","2","-maxdepth","2","-name","*_plan.json","-print0"] else [];
  _ = List.map verifyPlan plans;
  html = findFiles [root,"-name","*.html","-print0"];
  _ = List.map (fn path -> relative = suffixFrom path (Str.len root + 1);
    writePath path (finishPage relative (readPath path |> Str.replace "\r\n" "\n" |> Str.replace "\r" "\n"))) html;
  Unit.
> case args Unit of [dir] -> finish dir | _ -> error "usage: finish-manual.sol SITE-DIR".
