# Integrate model-only relative extrusion from the centered, diagonal Mk3 plates.
N = use "num".
J = use "sol/lib/json".
Map = use "std/map".
O = use "std/option".
wrap a = a  -  2 * N.pi * Num.floor (Num.div a (2 * N.pi)).
midpoint cmd x y xx yy i j =
  if cmd == "G2" or cmd == "G3" then
    start = N.atan2 (0 - j) (0 - i);
    finish = N.atan2 (yy - (y+j)) (xx - (x+i));
    sign = if cmd == "G3" then 1 else 0 - 1;
    delta = wrap (sign * (finish - start));
    sweep = sign * (if delta == 0 then 2*N.pi else delta);
    r = Num.div (N.hypot i j) sweep;
    (x+i+r*(N.sin (wrap (start+sweep)) - N.sin (wrap start)), y+j+r*(N.cos (wrap start) - N.cos (wrap (start+sweep))))
  else (Num.div (x+xx) 2, Num.div (y+yy) 2).
parseWord s = unwrap (Try.parseNum (if Str.startsWith "-." s then "-0" + Str.sub s 1 (Str.len s) else if Str.startsWith "." s then "0" + s else s)).
fields words = List.fold (fn acc w ->
  k = Str.sub w 0 1;
  if List.has k ["X","Y","Z","E","I","J"] then Map.insert k (parseWord (Str.sub w 1 (Str.len w))) acc else acc) Map.strings words.
get m k fallback = case Map.get k m of O.Some v -> v | O.None -> fallback.
advance state line =
  (x,y,z,active,e,sx,sy,sz) = state;
  if Str.startsWith "; FEATURE:" line then
    f = Str.trim (Str.sub line 10 (Str.len line)); (x,y,z,f != "Custom" and not (Str.startsWith "Support" f),e,sx,sy,sz)
  else if List.any (fn cmd -> Str.startsWith cmd line) ["G0 ","G1 ","G2 ","G3 "] then
    case Str.words (case Str.split 59 line of first :: rest -> first | [] -> "") of
      cmd :: args -> m = fields args; xx = get m "X" x; yy = get m "Y" y; zz = get m "Z" z;
        w = get m "E" 0;
        if active and w > 0 and (x != xx or y != yy or Map.member "I" m or Map.member "J" m) then
          (mx,my) = midpoint cmd x y xx yy (get m "I" 0) (get m "J" 0);
          (xx,yy,zz,active,e+w,sx+w*mx,sy+w*my,sz+w*(zz - 0.1))
        else (xx,yy,zz,active,e,sx,sy,sz)
    | [] -> state
  else state.
bounds b line = case Str.words line of
  ["vertex",a,c,d] -> (lx,hx,ly,hy,lz) = b; x = unwrap (Try.parseNum a); y = unwrap (Try.parseNum c); z = unwrap (Try.parseNum d);
    (Numeric.min x lx,Numeric.max x hx,Numeric.min y ly,Numeric.max y hy,Numeric.min z lz)
  | _ -> b.
arr xs = J.JArr (List.map J.JNum xs).
report dir scale wing tail =
  (lx,hx,ly,hy,lz) = List.fold bounds (1e9,0 - 1e9,1e9,0 - 1e9,1e9) (Str.lines (readPath "{dir}/glider.stl"));
  mac = Num.div (170 * scale) 3; area = 110*85*scale*scale/1000000.0;
  factor = 0.26*0.8*0.8*0.65; neutral = Num.div (wing+factor*tail) (1+factor);
  printers = List.map (fn p ->
    (_,_,_,_,e,sx,sy,sz) = List.fold advance (0,0,0,False,0,0,0,0) (Str.lines (readPath "{dir}/{p}/slice/plate_1.gcode"));
    bx = Num.div sx e  - 128 + Num.div (lx+hx) 2; by = Num.div sy e  - 128 + Num.div (ly+hy) 2;
    y = Num.div (by - bx) (Num.sqrt 2); mass = e*N.pi*0.875*0.875*1.26/1000.0;
    cl = Num.div (0.6*(y - wing)) (tail - wing);
    (p,J.JObj [("model_mass_g",J.JNum mass),("cg_xyz_mm",arr [Num.div (bx+by) (Num.sqrt 2),y,Num.div sz e + lz]),
      ("cg_aft_of_nose_mm",J.JNum (y+130.5*scale)),("static_margin_percent",J.JNum (100*(neutral - y)/mac)),
      ("speed_at_cl_06_m_s",J.JNum (Num.sqrt (2*mass*0.00980665/(1.225*area*0.6)))),
      ("trim_tail_incidence_deg",J.JNum (cl/(0.8*0.26*0.060) - 0.65*(0.6 - cl)/0.075))])) ["a1","x1c"];
  J.pretty (J.JObj [("mac_mm",J.JNum mac),("neutral_y_mm",J.JNum neutral),("area_dm2",J.JNum (area*100)),
    ("target_cg_aft_of_nose_mm",arr (List.map (fn f -> neutral - f*mac+130.5*scale) [0.15,0.10])),("printers",J.JObj printers)]).
