# Compare straight extruding moves labelled as bridges in the actual plates.
#   "$SOL" models/bolted-glider-light/supported/check-bridges.sol   (from the CoScad root or this directory)
N = use "../../lib/num".
R = use "../../lib/root".

# A G-code number as Python's float() reads it (".5", "-.5" and "1." included).
number s =
  case Try.parseNum s of
    Ok v -> v * 1.0
  | Err e ->
      t = if Str.startsWith "-." s then "-0" + N.drop 1 s else if Str.startsWith "." s then "0" + s else s;
      u = if Str.endsWith "." t then t + "0" else t;
      case Try.parseNum u of Ok v -> v * 1.0 | Err e2 -> error "bad G-code number: {s}".

hasAxis s = or (Str.contains "X" s) (or (Str.contains "Y" s) (Str.contains "E" s)).
# Fold a move word into (x, y, e), as re.findall(r"([XYE])(-?[\d.]+)") reads
# it. Words the regex would read differently (axes glued to other text,
# exponents, malformed numbers) are refused rather than misread.
axisValue word =
  rest = Str.slice word 1 (Str.len word);
  if or (hasAxis rest) (Str.contains "e" rest) then error "unexpected G-code word: {word}" else number rest.
field st word =
  (fx, fy, fe) = st;
  c = Str.at word 0;
  # A bare axis letter (or letter and sign) is no match for the regex.
  if or (Str.len word == 1) (and (Str.len word == 2) (Str.endsWith "-" word)) then st
  else if c == 88 then (axisValue word, fy, fe)
  else if c == 89 then (fx, axisValue word, fe)
  else if c == 69 then (fx, fy, axisValue word)
  else if hasAxis word then error "unexpected G-code word: {word}" else st.

# State: (x, y, bridge, longest, count >10 mm, mass).
step st line =
  (x, y, bridge, longest, over, mass) = st;
  br = if Str.startsWith "; FEATURE:" line then Str.contains "bridge" (Str.lower line) else bridge;
  moved = if or (Str.startsWith "G0 " line) (Str.startsWith "G1 " line) then
      command = case Str.split 59 line of first :: others -> first | [] -> "";
      # Missing X or Y keep the previous position; a missing E extrudes nothing.
      (a, b, e) = List.fold field (x, y, 0) (Str.words command);
      extrudes = and br (and (e > 0) (or (a != x) (b != y)));
      len = if extrudes then N.hypot (a - x) (b - y) else 0;
      (a, b, br,
       if extrudes then Numeric.max longest len else longest,
       if and extrudes (len > 10) then over + 1 else over, mass)
    else (x, y, br, longest, over, mass);
  if Str.contains "total filament weight [g]" line then
    (mx, my, mb, ml, mo, mm) = moved;
    parts = Str.split 58 line;
    (mx, my, mb, ml, mo, Str.trim (parts ! (List.len parts - 1)))
  else moved.

# Python's repr(round(v, 2)); an empty list reports the integer 0.
rounded v =
  s = N.stripZeros (N.fixed 2 v);
  if v == 0 - 1 then "0" else if Str.contains "." s then s else s + ".0".

> root = R.modelDir "models/bolted-glider-light/supported" "check-bridges.sol";
  List.map (fn printer -> List.map (fn folder ->
      text = readPath "{root}/../{folder}/{printer}/slice/plate_1.gcode";
      (x, y, b, longest, over, mass) = List.fold step (0.0, 0.0, False, 0 - 1, 0, "") (Str.lines text);
      print "{printer} {folder} max bridge move {rounded longest} mm; moves >10 mm {over} plate grams {mass}")
    ["tube/prototype", "supported"]) ["a1-wing-right", "p1s-wing-left"].
