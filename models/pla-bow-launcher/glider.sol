# 10 g printed PLA glider for the bow launcher. Dimensions in mm, masses in g.
#
# Glider frame: x forward, z up, y across. The origin is the closed end of the
# launch-hook slot, where the carriage pin's tip pushes. Fuselage bottom is
# z = 3, which rests in the groove of the carriage mast cap.
#
# Layout: profile fuselage (boom 1.2 mm, pod 1.6 mm, fin 0.4 mm) with a nose
# pod whose rear has the hook slot; flat 0.4 mm wing on a saddle that sets
# 2 deg incidence and 6 deg dihedral a side; flat 0.4 mm stab through a slot in
# the tail boom.
#
# Writes the parts, renders them with CoScad, computes mass and CG from the
# meshes (solid PLA), and places the wing for a 10% static margin. Writes
# glider.json and glider.assemble (build.sol reads glider.json).
#
#   "$SOL" models/pla-bow-launcher/glider.sol [BALLAST_G] [--coscad PATH]
# The committed design uses 1.0 g of M3 nose trim. The compiler defaults to
# $COSCAD, then this checkout's dist-newstyle build (it needs `poly`).
N = use "../lib/num".
R = use "../lib/root".
P = use "sol/lib/proc".
C = use "sol/lib/cli".

rho = 0.00124.

f v = N.trim v.
half a b = Num.div (a + b) 2.
neg x = 0 - x.
box x0 x1 y0 y1 z0 z1 =
  "(box {f (x1 - x0)} {f (y1 - y0)} {f (z1 - z0)} |> move {f (half x0 x1)} {f (half y0 y1)} {f (half z0 z1)})".
poly pts h = "(⮕ {f h} (poly " + Str.join " " (List.map (fn p -> (a, b) = p; "{f a} {f b}") pts) + "))".
un parts = "(" + Str.join " ⊕ " parts + ")".
dif a parts = "(" + a + Str.join "" (List.map (fn q -> " ⊖ " + q) parts) + ")".
ycyl r y0 y1 x z = "(ycyl {f r} {f (y1 - y0)} |> move {f x} {f (half y0 y1)} {f z})".
# Polygon in the x-z plane, thickness y0..y1: θ 90 maps (X,Y,Z) -> (X,-Z,Y), so
# the poly's Y becomes z and the extrusion becomes -y.
xz pts y0 y1 = "((θ 90 {poly pts (y1 - y0)}) |> move 0 {f y1} 0)".
# Polygon in the y-z plane, extruded along x from x0 to x1.
yzRib pts x0 x1 = "((ω 90 (θ 90 {poly pts (x1 - x0)})) |> move {f x0} 0 0)".

# ---- fixed geometry ----------------------------------------------------------
tPod = 1.6. tFus = 1.2. tFin = 0.4.   # nose pod, boom, fin
zBot = 3.0. zTop = 10.0.              # fuselage bottom (sits in the mast cap groove) and top edge
podBot = 0.0 - 3.65. podRear = 0.0 - 7.8.   # nose pod; its rear edge carries the hook slot
slotHalf = 1.65.                      # 3.3 mm slot for the 3.0 mm pin
nose = 62.0.
tail = 0.0 - 118.0.
stabX0 = 0.0 - 116.0. stabX1 = 0.0 - 94.0.
stabZ0 = 7.0. stabZ1 = 7.6.           # 0.6 mm slot for the 0.4 mm stab
stabT = 0.4.
wingT = 0.4. sparH = 0.6.
chord = 36.0. span = 230.0.
incidence = 2.0. dihedral = 6.0.
zSaddleMid = 12.3.                    # wing underside at mid-chord on the centreline
ballastX = [44.0, 52.0].

# Fuselage top line: flat at 10 from the nose to x = -30, then down to 9 at the tail.
topZ x = if x >= 0 - 30 then zTop else zTop - Num.div ((0 - 30 - x) * 1.0) (0 - 30 - tail).

podOutline = [(podRear, podBot), (nose - 12, podBot), (nose - 4, 0 - 1.5), (nose, 3), (nose - 1.5, 7.5), (nose - 7, zTop)].
fuselage =
  body = podOutline + [(0 - 30, zTop), (tail, topZ tail), (tail, zBot), (podRear, zBot)];
  fin = [(tail, topZ tail - 1.5), (tail, 34.0), (tail + 7, 36.0), (tail + 30, topZ (tail + 30) - 1.5)];
  pod = podOutline + [(podRear, zTop)];
  shape = un [xz body (neg (Num.div tFus 2)) (Num.div tFus 2), xz pod (neg (Num.div tFus 2)) (neg (Num.div tFus 2) + tPod),
              xz fin (neg (Num.div tFus 2)) (neg (Num.div tFus 2) + tFin)];
  chamfer = [(podRear - 0.01, neg slotHalf - 0.8), (podRear + 1.2, neg slotHalf), (podRear + 1.2, slotHalf), (podRear - 0.01, slotHalf + 0.8)];
  cuts = [box (podRear - 2) 0 (0 - 2) 2 (neg slotHalf) slotHalf,                   # hook slot
          # entry chamfer so the slot finds the pin
          "((θ 90 {poly chamfer 4}) |> move 0 2 0)",
          box stabX0 stabX1 (0 - 2) 2 stabZ0 stabZ1]                               # stab slot
         + List.map (fn x -> ycyl 1.6 (0 - 2) 2 x 1.0) ballastX;                  # M3 trim weights
  dif shape cuts.

# Flat, mid-chord at x = 0, underside z = 0. Spar ridge on top at 30% chord,
# stopped short of the centre so the plate bends into the saddle's dihedral.
wing =
  c2 = Num.div chord 2; s2 = Num.div span 2;
  pts = [(c2, neg (s2 - 25)), (c2, s2 - 25), (c2 - 10, s2), (neg c2, s2), (neg c2, neg s2), (c2 - 10, neg s2)];
  xs = c2 - 0.3 * chord;
  un [poly pts wingT, box (xs - 0.6) (xs + 0.6) 10 (s2 - 12) (wingT - 0.01) (wingT + sparH),
      box (xs - 0.6) (xs + 0.6) (neg (s2 - 12)) (0 - 10) (wingT - 0.01) (wingT + sparH)].

stab = poly [(11, 0 - 28), (11, 28), (4, 35), (0 - 11, 35), (0 - 11, 0 - 35), (4, 0 - 35)] stabT.

# Three ribs with a V top (dihedral) rising toward the front (incidence),
# joined by keel walls that straddle the fuselage top edge.
saddle xLe =
  xMid = xLe - Num.div chord 2;
  zc = fn x -> zSaddleMid + (x - xMid) * N.tan (N.radians incidence);
  w = 9.0; k = N.tan (N.radians dihedral);
  ribs = List.map (fn r -> (x0, x1) = r; z = zc (Num.div (x0 + x1) 2);
      yzRib [(neg w, 6.0), (w, 6.0), (w, z - w * k), (0, z), (neg w, z - w * k)] x0 x1)
    [(xLe - 1.2, xLe), (xMid - 0.6, xMid + 0.6), (xLe - chord, xLe - chord + 1.2)];
  g = Num.div tFus 2 + 0.025;
  keel = [box (xLe - chord) xLe g (g + 0.8) 6.0 10.6, box (xLe - chord) xLe (neg g - 0.8) (neg g) 6.0 10.6,
          box (xLe - chord) xLe (neg g - 0.8) (g + 0.8) 10.0 10.6];
  dif (un (ribs + keel)) [box (xLe - chord - 1) (xLe + 1) (neg g) g 5.0 10.0].

notes = [("glider-fuselage", "// Glider frame. Print on its side, fin side down (fwd).\n"),
         ("glider-wing", "// Flat; bend into the saddle's 6 deg dihedral and glue.\n"),
         ("glider-stab", "// Slides through the tail boom slot; glue centred.\n"),
         ("glider-saddle", "// Glider frame. Print keel down. Glue on the fuselage top edge.\n")].
noteFor name = List.fold (fn acc r -> (n, t) = r; if n == name then t else acc) "" notes.
sourceText name body = noteFor name + "main = {body}\n".

# ---- rendering and mass properties --------------------------------------------
runCmd argv = case Proc.streamNow (P.spec argv) of
    Ok 0 -> Unit
  | Ok code -> error "{Str.join " " argv} exited {code}"
  | Err why -> error why.
# Stage the source on disk now (Sol writes otherwise land at commit) in a fresh
# directory, compile it, and return the stage directory and the ASCII STL text.
render compiler name body =
  stage = tempDir "coscad-glider";
  path = "{stage}/{name}.coscad";
  pipe = PipelineSpec [P.spec ["cat"]] (PipeBytes (unwrap (Utf8.encode (sourceText name body)))) (PipeFileNew path) PipeOutputInherit 0 PipeFail;
  _ = case Proc.pipeline pipe of Ok (PipelineResult 0 _ _ _) -> Unit | other -> error "cannot stage {path}: {other}";
  _ = runCmd [compiler, "stl", path, "-o", "{stage}/{name}.stl"];
  (stage, readPath "{stage}/{name}.stl").
num s = unwrap (Try.parseNum s).
vertices stl = List.concat (List.map (fn line -> case Str.words line of
    ["vertex", a, b, c] -> [(num a, num b, num c)]
  | _ -> []) (Str.lines stl)).
triples vs = case vs of
    a :: b :: c :: rest -> (a, b, c) :: triples rest
  | _ -> [].
triples : (vs : List (Int, Int, Int) | measure vs) -> List ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int)).
# Volume and centroid, summed in the same order as the Python original.
massProps staged =
  (stage, stl) = staged;
  (vol, cx, cy, cz) = List.fold (fn acc t -> (vv, sx, sy, sz) = acc; (a, b, c) = t;
      (a0, a1, a2) = a; (b0, b1, b2) = b; (c0, c1, c2) = c;
      v = Num.div (a0 * (b1 * c2 - b2 * c1) - a1 * (b0 * c2 - b2 * c0) + a2 * (b0 * c1 - b1 * c0)) 6;
      (vv + v, sx + Num.div (v * (a0 + b0 + c0)) 4, sy + Num.div (v * (a1 + b1 + c1)) 4, sz + Num.div (v * (a2 + b2 + c2)) 4))
    (0.0, 0.0, 0.0, 0.0) (triples (vertices stl));
  {volume = vol, mass = vol * rho, cgx = Num.div cx vol, stage = stage}.

# ---- JSON in Python's json.dumps(indent=1) layout -------------------------------
rounded n x = N.repr (unwrap (Try.parseNum (N.fixed n x))).

glider dir compiler mBallast =
  pFus = massProps (render compiler "glider-fuselage" fuselage);
  pWing = massProps (render compiler "glider-wing" wing);
  pStab = massProps (render compiler "glider-stab" stab);
  mGlue = 0.3;
  sm = 0.1;
  sW = chord * span - 2 * 0.5 * 10 * 25;
  sH = 22 * 70 - 2 * 0.5 * 7 * 7;
  cgFrac = fn xLe -> lh = (xLe - 0.25 * chord) - (stabX1 - 0.25 * 22);
    vh = Num.div (sH * lh) (sW * chord); 0.25 + 0.48 * vh - sm;     # neutral point minus the margin (rough)
  fixedMoment = pFus.mass * pFus.cgx + Num.div (pStab.mass * (stabX0 + stabX1)) 2 + Num.div (mBallast * List.fold (fn a x -> a + x) 0 ballastX) 2;
  # six rounds: re-render the saddle at the current leading edge, then solve
  # for the leading edge with the CG fraction evaluated at the previous one
  # (as in the original, the published saddle is the last one rendered, i.e.
  # at the leading edge before the final update)
  (xLe, pSaddle, mTotal, xSaddle) = List.fold (fn st i -> (x, ps, mt, xs) = st;
      p = massProps (render compiler "glider-saddle" (saddle x));
      mMove = pWing.mass + p.mass + mGlue;
      m = pFus.mass + pStab.mass + mBallast + mMove;
      k = cgFrac x;
      (Num.div (fixedMoment - Num.div (mMove * chord) 2 + k * chord * m) (m - mMove), p, m, x))
    (0.0, pWing, 0.0, 0.0) (List.range 1 6);
  cgf = cgFrac xLe;
  xCg = xLe - cgf * chord;
  lh = (xLe - 0.25 * chord) - (stabX1 - 0.25 * 22);
  vh = Num.div (sH * lh) (sW * chord);
  wgt = Num.div mTotal 1000 * 9.81;
  vGlide = Num.sqrt (Num.div (2 * wgt) (1.225 * sW * 0.000001 * 0.6));
  json = "\{\n \"parts\": \{\n"
    + "  \"glider-fuselage\": {rounded 2 pFus.mass},\n  \"glider-wing\": {rounded 2 pWing.mass},\n"
    + "  \"glider-stab\": {rounded 2 pStab.mass},\n  \"glider-saddle\": {rounded 2 pSaddle.mass}\n \},\n"
    + " \"ballast_g\": {N.repr mBallast},\n \"glue_g\": {N.repr mGlue},\n \"total_g\": {rounded 2 mTotal},\n"
    + " \"wing_le_x\": {rounded 1 xLe},\n \"cg_x\": {rounded 1 xCg},\n"
    + " \"wing_area_cm2\": {rounded 1 (Num.div sW 100)},\n \"wing_loading_g_dm2\": {rounded 1 (Num.div mTotal (Num.div sW 10000))},\n"
    + " \"tail_volume\": {rounded 2 vh},\n \"cg_chord_fraction\": {rounded 3 cgf},\n \"static_margin\": {N.repr sm},\n"
    + " \"glide_speed_CL06\": {rounded 1 vGlide}\n\}";
  _ = print json;
  xw = xLe - Num.div chord 2;
  assemble = "// 10 g glider in its own frame (origin = hook slot end). Wing shown flat at 2 deg incidence;\n"
    + "// in use it is bent into the saddle's 6 deg dihedral.\nplate 256 256 6\n"
    + "fuselage ← glider-fuselage.coscad ×1 ▽fwd material=pla\nwing ← glider-wing.coscad ×1 ▽bot material=pla\n"
    + "stab ← glider-stab.coscad ×1 ▽bot material=pla\nsaddle ← glider-saddle.coscad ×1 ▽bot material=pla\n\n"
    + "fixed fuselage 0 0 0 0 0 0\nfixed saddle 0 0 0 0 0 0\n"
    + "fixed wing {f xw} 0 {f zSaddleMid} 0 {f (neg incidence)} 0\n"
    + "fixed stab {f (Num.div (stabX0 + stabX1) 2)} 0 {f (stabZ0 + Num.div (stabZ1 - stabZ0 - stabT) 2)} 0 0 0\n";
  # publish: meshes copied now (before any queued write), then sources and
  # reports through the transaction
  _ = List.map (fn r -> (name, stage) = r; case Fs.copyPublishNow "{stage}/{name}.stl" "{dir}/{name}.stl" 65536 True of
      Ok size -> Unit | Err why -> error (str why))
    [("glider-fuselage", pFus.stage), ("glider-wing", pWing.stage), ("glider-stab", pStab.stage), ("glider-saddle", pSaddle.stage)];
  _ = List.map (fn r -> (name, body) = r; writePath "{dir}/{name}.coscad" (sourceText name body))
        [("glider-fuselage", fuselage), ("glider-wing", wing), ("glider-stab", stab), ("glider-saddle", saddle xSaddle)];
  _ = writePath "{dir}/glider.json" (json + "\n");
  writePath "{dir}/glider.assemble" assemble.

> opts = unwrap (C.parse [C.Option "coscad" "" C.One] (args Unit));
  dir = R.modelDir "models/pla-bow-launcher" "glider.sol";
  envCompiler = case unwrap (Env.get "COSCAD") of EnvText p -> p | EnvMissing -> "{dir}/../../dist-newstyle/build/aarch64-osx/ghc-9.8.2/coscad-1.6.0.0/x/coscad/build/coscad/coscad";
  ballast = case C.operands opts of [b] -> unwrap (Try.parseNum b) | [] -> 0.0 | _ -> error "usage: glider.sol [BALLAST_G] [--coscad PATH]";
  glider dir (C.valueOr envCompiler "coscad" opts) ballast.
