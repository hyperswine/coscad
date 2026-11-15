# Check extrusion coverage of the spiral centreline at all six strip layers.
# Run from the CoScad root or this directory: sol check-spring.sol
#
# Each extruding G-code move is a capsule: the segment buffered by half its
# line width plus 0.03 mm. A point is covered by a layer's union when it lies
# within the radius of some capsule; the hub and anchor eye are connected when
# their capsules join one union-find set, where two capsules are joined when
# their segments lie within the sum of their radii (sweep and prune on x
# within 2 mm bands of y).
# shapely, used before, drew the caps with 16 chords per quarter circle inside
# these exact capsules, so the two agree except within 0.001 mm of a boundary.
N = use "../../lib/num".
R = use "../../lib/root".

# The plate on the bed, the spiral on the plate, and the two test points.
spiralX = 54.
spiralR0 = 4.5.
spiralGrow = 10.5.
anchorX = 16.5.
anchorBore = 2.325.
hubTestX = 58.5.
anchorTestX = 34.8.

# Python float() accepts ".4", "-.8" and "1."; Try.parseNum does not.
pnum s =
  n = Str.len s;
  c0 = Str.at s 0;
  # (and/or evaluate both arguments, so guard the second index with if.)
  minusPoint = if and (c0 == 45) (n > 1) then Str.at s 1 == 46 else False;
  t0 = if c0 == 46 then "0" + s else if minusPoint then "-0" + N.drop 1 s else s;
  t = if Str.at s (n - 1) == 46 then t0 + "0" else t0;
  unwrap (Try.parseNum t).

# Centre the plate's xy bounding box on the 256 mm bed, as the slicer did.
plateOffset stl =
  verts = List.filter (fn ws -> case ws of w :: _ -> w == "vertex" | _ -> False) (List.map Str.words (Str.lines stl));
  xs = List.map (fn ws -> pnum (ws ! 1)) verts;
  ys = List.map (fn ws -> pnum (ws ! 2)) verts;
  mid vs = Num.div (List.fold Numeric.min (vs ! 0) vs + List.fold Numeric.max (vs ! 0) vs) 2;
  (128 - mid xs, 128 - mid ys).

# Capsule: (xl, xh, yl, yh, x1, y1, x2, y2, r), its box grown by r.
capsule x1 y1 x2 y2 r = (Numeric.min x1 x2 - r, Numeric.max x1 x2 + r, Numeric.min y1 y2 - r, Numeric.max y1 y2 + r, x1, y1, x2, y2, r).
moveFields code st = List.fold (fn acc w ->
    (a, b, c, e) = acc;
    k = Str.at w 0;
    if k == 88 then (pnum (N.drop 1 w), b, c, e)
    else if k == 89 then (a, pnum (N.drop 1 w), c, e)
    else if k == 90 then (a, b, pnum (N.drop 1 w), e)
    else if k == 69 then (a, b, c, pnum (N.drop 1 w)) else acc) st (List.drop 1 (Str.words code)).
gmove line st =
  (x, y, z, width, segs) = st;
  if or (Str.startsWith "G0 " line) (Str.startsWith "G1 " line) then
    cut = Str.indexOf ";" line;
    code = if cut < 0 then line else N.take cut line;
    (a, b, c, e) = moveFields code (x, y, z, 0);
    keep = and (e > 0) (and (or (a != x) (b != y)) (and (c >= 0.19) (c <= 1.21)));
    segs2 = if keep then (Num.round (c * 100), capsule x y a b (Num.div width 2 + 0.03)) :: segs else segs;
    (a, b, c, width, segs2)
  else st.
# Extruding moves between z 0.19 and 1.21, as (layer z in hundredths, capsule).
extrusions gcode =
  (x, y, z, width, segs) = List.fold (fn st line ->
      (x, y, z, width, segs) = st;
      c0 = if Str.len line > 2 then Str.at line 0 else 0;
      if c0 == 71 then gmove line st else if c0 != 59 then st
      else if Str.startsWith "; LINE_WIDTH:" line then (x, y, z, pnum (Str.trim (Str.split 58 line ! 1)), segs)
      else st) (0.0, 0.0, 0.0, 0.42, []) (Str.lines gcode);
  segs.

# Squared distance from a point to a segment.
pointSeg2 px py x1 y1 x2 y2 =
  dx = x2 - x1; dy = y2 - y1;
  t = N.clamp 0 1 (Num.div ((px - x1) * dx + (py - y1) * dy) (dx * dx + dy * dy));
  ex = px - (x1 + t * dx); ey = py - (y1 + t * dy);
  ex * ex + ey * ey.
covers c px py = (xl, xh, yl, yh, x1, y1, x2, y2, r) = c;
  if and (and (px >= xl) (px <= xh)) (and (py >= yl) (py <= yh)) then pointSeg2 px py x1 y1 x2 y2 <= r * r else False.
cross ax ay bx by cx cy = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax).
# Within the sum of the radii: an endpoint near the other segment, else a crossing.
touches c d =
  (axl, axh, ayl, ayh, a1, b1, a2, b2, ar) = c;
  (bxl, bxh, byl, byh, c1, d1, c2, d2, br) = d;
  rr = (ar + br) * (ar + br);
  if pointSeg2 c1 d1 a1 b1 a2 b2 <= rr then True
  else if pointSeg2 c2 d2 a1 b1 a2 b2 <= rr then True
  else if pointSeg2 a1 b1 c1 d1 c2 d2 <= rr then True
  else if pointSeg2 a2 b2 c1 d1 c2 d2 <= rr then True
  else and (cross a1 b1 a2 b2 c1 d1 * cross a1 b1 a2 b2 c2 d2 < 0) (cross c1 d1 c2 d2 a1 b1 * cross c1 d1 c2 d2 a2 b2 < 0).

# Sweep entries (key, band, xl, xh, yl, yh, capsule, index): one per 2 mm
# band of y the capsule's box overlaps, sorted by band and then by the box's
# low x (the key folds both into one number for the sort).
band = 2.
before e f = (k, b, xl, xh, yl, yh, c, i) = e; (k2, b2, xl2, xh2, yl2, yh2, c2, i2) = f; k < k2.
merge : unsafe List (Int, Int, Int, Int, Int, Int, (Int, Int, Int, Int, Int, Int, Int, Int, Int), Int) -> List (Int, Int, Int, Int, Int, Int, (Int, Int, Int, Int, Int, Int, Int, Int, Int), Int) -> List (Int, Int, Int, Int, Int, Int, (Int, Int, Int, Int, Int, Int, Int, Int, Int), Int).
merge xs ys = case xs of
    [] -> ys
  | x :: xt -> case ys of
      [] -> xs
    | y :: yt -> if before y x then y :: merge xs yt else x :: merge xt ys.
msort : unsafe List (Int, Int, Int, Int, Int, Int, (Int, Int, Int, Int, Int, Int, Int, Int, Int), Int) -> List (Int, Int, Int, Int, Int, Int, (Int, Int, Int, Int, Int, Int, Int, Int, Int), Int).
msort xs =
  n = List.len xs;
  if n < 2 then xs else merge (msort (List.take (n / 2) xs)) (msort (List.drop (n / 2) xs)).

find : unsafe Int -> Vector Int -> (Int, Vector Int).
find i v =
  (p, v2) = Vec.get i v;
  if p == i then (i, v2) else
    (r, v3) = find p v2;
    (r, Vec.set i r v3).
# Join capsule c (band b, box right edge xh, box rows yl..yh, set root ra)
# with every later entry in its band whose box starts before c's ends. The
# distance test is skipped when both are already in one set, which on a dense
# layer is most of the time; linking c's root under the other keeps ra current.
scan : unsafe Int -> Int -> Int -> Int -> (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Int -> List (Int, Int, Int, Int, Int, Int, (Int, Int, Int, Int, Int, Int, Int, Int, Int), Int) -> Vector Int -> Vector Int.
scan b xh yl yh c ra rest v = case rest of
    [] -> v
  | e :: t ->
      (k2, b2, xl2, xh2, yl2, yh2, c2, j) = e;
      if or (b2 != b) (xl2 > xh) then v
      else if or (yl2 > yh) (yl > yh2) then scan b xh yl yh c ra t v
      else
        (rb, v2) = find j v;
        if ra == rb then scan b xh yl yh c ra t v2
        else if touches c c2 then scan b xh yl yh c rb t (Vec.set ra rb v2)
        else scan b xh yl yh c ra t v2.
sweep : unsafe List (Int, Int, Int, Int, Int, Int, (Int, Int, Int, Int, Int, Int, Int, Int, Int), Int) -> Vector Int -> Vector Int.
sweep xs v = case xs of
    [] -> v
  | h :: t -> (k, b, xl, xh, yl, yh, c, i) = h; (ra, v2) = find i v; sweep t (scan b xh yl yh c ra t v2).
firstCovering px py indexed = List.fold (fn acc ci -> (c, i) = ci; if and (acc < 0) (covers c px py) then i else acc) (0 - 1) indexed.
# (List.concat is quadratic in the number of lists, so prepend instead.)
bandEntries acc ci = (c, i) = ci; (xl, xh, yl, yh, x1, y1, x2, y2, r) = c;
  List.fold (fn es b -> (b * 10000 + xl, b, xl, xh, yl, yh, c, i) :: es) acc (List.range (Num.floor (Num.div yl band)) (Num.floor (Num.div yh band))).
# Are the capsules under the two points in one connected piece of the union?
connectedAt : unsafe List (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Int -> Int -> Int -> Int -> Bool.
connectedAt caps ax ay bx by =
  n = List.len caps;
  indexed = List.zip caps (List.range 0 (n - 1));
  ia = firstCovering ax ay indexed;
  ib = firstCovering bx by indexed;
  if or (ia < 0) (ib < 0) then False else
    v = sweep (msort (List.fold bandEntries [] indexed)) (Vec.fromList (List.range 0 (n - 1)));
    (ra, v2) = find ia v;
    (rb, v3) = find ib v2;
    _ = Vec.free v3;
    ra == rb.

# First capsule covering a point, trying the previous sample's capsule first.
firstHit : unsafe Int -> Int -> List (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> List (Int, Int, Int, Int, Int, Int, Int, Int, Int).
firstHit px py cs = case cs of
    [] -> []
  | c :: t -> if covers c px py then [c] else firstHit px py t.

layerReport : unsafe List (Int, (Int, Int, Int, Int, Int, Int, Int, Int, Int)) -> Int -> Int -> Int -> (Int, Bool, Int, Int).
layerReport segs ox oy key =
  caps = List.map (fn kc -> (k, c) = kc; c) (List.filter (fn kc -> (k, c) = kc; k == key) segs);
  cx = spiralX + ox;
  reach = spiralR0 + spiralGrow + 1;
  near = List.filter (fn c -> (xl, xh, yl, yh, x1, y1, x2, y2, r) = c;
      and (and (xl <= cx + reach) (xh >= cx - reach)) (and (yl <= oy + reach) (yh >= oy - reach))) caps;
  (tested, missing, last) = List.fold (fn st i ->
      (tested, missing, last) = st;
      t = Num.div (3 * N.pi * i) 1000; r = spiralR0 + Num.div (spiralGrow * i) 1000;
      # Anchor bore and perimeter transition.
      if N.hypot (r * N.cos t + anchorX) (r * N.sin t) < anchorBore then st else
        px = cx + r * N.cos t; py = oy + r * N.sin t;
        hit = if List.any (fn c -> covers c px py) last then last else firstHit px py near;
        (tested + 1, if hit == [] then missing + 1 else missing, hit)) (0, 0, []) (List.range 0 1000);
  connected = connectedAt caps (ox + hubTestX) oy (ox + anchorTestX) oy;
  z = Num.div key 100;
  _ = if connected then Unit else error "AssertionError: {N.repr z}";
  (z, connected, tested, missing).

entryJson e = (z, connected, tested, missing) = e;
  "  \{\n    \"layer_z_mm\": " + N.repr z + ",\n    \"hub_to_anchor_connected\": " + (if connected then "true" else "false")
  + ",\n    \"centreline_samples\": " + str tested + ",\n    \"uncovered_samples\": " + str missing + "\n  \}".

main : unsafe Unit -> Unit.
main u =
  root = R.modelDir "models/captive-spring-demo/larger" "check-spring.sol";
  (ox, oy) = plateOffset (readPath "{root}/plate.stl");
  segs = extrusions (readPath "{root}/a1-wing-right/slice/plate_1.gcode");
  report = List.map (layerReport segs ox oy) [20, 40, 60, 80, 100, 120];
  text = "[\n" + Str.join ",\n" (List.map entryJson report) + "\n]";
  _ = print text;
  _ = writePath "{root}/spiral-coverage.json" text;
  # Like the Python original, keep the report even when the check fails.
  if List.all (fn e -> (z, c, t, m) = e; m == 0) report then Unit
  else _ = unwrap (IO.stderrText "AssertionError: uncovered centreline samples\n"); Script.exit 1.
> main Unit.
