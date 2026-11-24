# Run from this directory with baseline and candidate plate G-code paths.
F = use "../../lib/glider-flight".
N = use "../../lib/num".
Map = use "std/map".
factor = N.pi * 0.875 * 0.875 * 1.26 / 1000.0.
selected = 11.
segment cmd x y xx yy m =
  i = F.get m "I" 0; j = F.get m "J" 0;
  arc = cmd == "G2" or cmd == "G3";
  sign = if cmd == "G3" then 1 else 0 - 1;
  sweep = F.wrap (sign * (N.atan2 (yy - (y+j)) (xx - (x+i)) - N.atan2 (0 - j) (0 - i)));
  r = N.trim (N.hypot i j);
  end = if arc then "A " + r + " " + r + " 0 " + (if sweep > N.pi then "1" else "0") + (if sign == 1 then " 1 " else " 0 ") else "L ";
  "M " + N.trim x + " " + N.trim y + " " + end + N.trim xx + " " + N.trim yy.
step drawing state line =
  (x,y,z,f,mass,sparse,paths) = state;
  if Str.startsWith "; FEATURE:" line then (x,y,z,Str.trim (Str.sub line 10 (Str.len line)),mass,sparse,paths)
  else if Str.startsWith "; Z_HEIGHT:" line then (x,y,unwrap (Try.parseNum (Str.trim (Str.sub line 11 (Str.len line)))),f,mass,sparse,paths)
  else if List.any (fn s -> Str.startsWith s line) ["G0 ","G1 ","G2 ","G3 "] then
    case Str.words (case Str.split 59 line of a :: b -> a | [] -> "") of
      cmd :: args -> m = F.fields args; xx = F.get m "X" x; yy = F.get m "Y" y; e = F.get m "E" 0;
        lay = e > 0 and (x != xx or y != yy or Map.member "I" m or Map.member "J" m) and z > 0 and f != "Custom" and not (Str.startsWith "Support" f);
        infill = f == "Sparse infill";
        col = if infill then "#d45a12" else if Str.contains "wall" f then "#233f62" else "#9cacc0";
        added = if drawing and lay and z == selected then ("<path stroke=\"" + col + "\" d=\"" + segment cmd x y xx yy m + "\"/>") :: paths else paths;
        (xx,yy,z,f,mass + (if lay then e else 0),sparse + (if lay and infill then e else 0),added)
    | [] -> state
  else state.
panel name offset file =
  lines = Str.lines (readPath file);
  (_,_,_,_,model,sparse,_) = List.fold (step False) (0,0,0,"Custom",0,0,[]) lines;
  selectedLines = List.fold (fn acc line -> (on,rev) = acc; active = if Str.startsWith "; Z_HEIGHT:" line then Str.trim line == "; Z_HEIGHT: 11" else on; (active,if active then line :: rev else rev)) (False,[]) lines;
  (_,layerRev) = selectedLines;
  (_,_,_,_,_,_,paths) = List.fold (step True) (0,0,0,"Custom",0,0,[]) (List.rev layerRev);
  _ = print (name,N.fixed 3 (model*factor),N.fixed 3 (sparse*factor),List.len paths);
  "<text x=\"" + str (offset+12) + "\" y=\"24\">" + name + "</text><g transform=\"translate(" + str offset + " 264) scale(1 -1)\" fill=\"none\" stroke-width=\"0.45\">" + Str.join "\n" (List.rev paths) + "</g>".
> case args Unit of
  [old,new] -> a = panel "Gyroid" 0 old; b = panel "Lightning" 240 new;
    writePath "wing-layer.svg" ("<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 500 285\" width=\"1000\" height=\"570\"><rect width=\"500\" height=\"285\" fill=\"white\"/><g font-family=\"sans-serif\" font-size=\"13\">" + a + b + "<text x=\"12\" y=\"277\" font-size=\"9\">Z = 11 mm · Orange: sparse infill · Navy: walls · Gray: solid skin and other model paths</text></g></svg>")
  | _ -> error "expected two G-code files".
