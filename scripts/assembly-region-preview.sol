# Render an XY preview report from "coscad stress ... --bonded ... --preview".
J = use "sol/lib/json".
field key obj = unwrap (J.get key obj).
array key obj = unwrap (J.items (field key obj)).
number j = unwrap (J.num j).
text key obj = unwrap (J.text (field key obj)).
xml s = s |> Str.replace "&" "&amp;" |> Str.replace "<" "&lt;" |> Str.replace ">" "&gt;" |> Str.replace "\"" "&quot;" |> Str.replace "'" "&#x27;".
cellXY pair = case unwrap (J.items pair) of
    [x,y] -> (number x,number y)
  | _ -> error "expected an XY cell corner".
bodyCells body = List.map cellXY (array "xy_footprint_mm" body).
height body = number (field "xy_cell_mm" body).
bounds body = h = height body;
  List.map (fn point -> (x,y) = point; (x,y,x+h,y+h)) (bodyCells body).
merge first second =
  (x,y,r,t) = first; (a,b,c,d) = second;
  (Numeric.min x a, Numeric.min y b, Numeric.max r c, Numeric.max t d).
viewBox bodies = case List.concat (List.map bounds bodies) of
    first :: rest -> (l,b,r,t) = List.fold merge first rest;
      "{l - 2} {0 - t - 2} {r - l + 4} {t - b + 4}"
  | [] -> error "empty geometry footprint".
bodyPath index body =
  h = height body;
  path = Str.join " " (List.map (fn point -> (x,y) = point; "M{x},{0 - y}h{h}v{0 - h}h{0 - h}z") (bodyCells body));
  color = case (index - 3 * (index / 3)) of 0 -> "#9bc8d2" | 1 -> "#e4ba94" | _ -> "#b6c6a8";
  title = xml (text "instance" body);
  "<path d=\"{path}\" fill=\"{color}\" opacity=\".65\"><title>{title}</title></path>".
corner j = case unwrap (J.items j) of
    [x,y,z] -> "{number x},{0 - number y}"
  | _ -> error "expected XYZ face corner".
regionPolygons region =
  role = text "role" region;
  color = case role of "support" -> "#225baf" | "load" -> "#bd3027" | _ -> "#188148";
  title = xml role + " on " + xml (text "instance" region);
  List.map (fn face ->
    points = Str.join " " (List.map corner (unwrap (J.items face)));
    "<polygon points=\"{points}\" fill=\"{color}\" stroke=\"{color}\" stroke-width=\".5\"><title>{title}</title></polygon>") (array "faces_mm" region).
render preview =
  bodies = array "geometry" preview;
  header = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"{viewBox bodies}\">";
  paths = List.zip (List.range 0 (List.len bodies - 1)) bodies |> List.map (fn p -> (i,b) = p; bodyPath i b);
  polygons = List.concat (List.map regionPolygons (array "regions" preview));
  Str.join "\n" (List.concat [[header,"<title>Assembly regions in the analysis XY plane</title>"],paths,polygons,["</svg>",""]]).
renderFile input output =
  # POSIX test follows symlinks and compares inode identity, including hard links.
  ProcessResult status _ message = unwrap (Proc.query (ProcessSpec ["test",input,"-ef",output] "" [] "" 0));
  case status of
    0 -> error "choose a separate output file"
  | 1 -> report = unwrap (J.parse (readPath input)); writePath output (render (field "preview" report))
  | _ -> error "file comparison failed: {message}".
> case args Unit of [input,output] -> renderFile input output | _ -> error "usage: assembly-region-preview.sol REPORT SVG".
