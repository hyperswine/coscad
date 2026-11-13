# Generate both screw profiles, preserve the globe relief, and export each view.
C = use "sol/lib/cli".
J = use "sol/lib/json".
P = use "sol/lib/proc".
W = use "../workflow".
pitch = 6.0.
step = 7.5.
perTurn = 48.
# Batch the platform libm sin/cos calls and fixed-six-place formatting. The
# signed zeros in this text are part of the existing source hashes.
formatPoints points =
  awk = "\{ a=$2*(atan2(0,-1)/180); printf(\"χ %.6f (ψ %.6f (ζ %.6f (point)))\\n\", $1*cos(a), $1*sin(a), $3) \}";
  text = Str.join "\n" (List.map (fn p -> (radius,angle,z) = p; "{radius} {angle} {z}") points);
  spec = P.spec ["awk",awk] |> P.withEnv "LC_ALL" "C" |> P.withStdin (text + "\n");
  Str.lines (unwrap (P.output (Proc.query spec))) |> List.filter (fn line -> line != "").
helix name clearance axial =
  corners = [(9.95 + clearance,-2.2 - axial),(12 + clearance,-0.2 - axial),
             (12 + clearance,0.2 + axial),(9.95 + clearance,2.2 + axial)];
  stations = List.range (0 - perTurn) (3 * perTurn - 1);
  pointRow = fn a -> fn c -> (radius,dz) = c; (radius,a,Numeric.div (pitch * a) 360 + dz);
  segment i = List.map (fn a -> List.map (pointRow a) corners) [i * step,(i + 1) * step] |> List.concat;
  coords = List.map segment stations |> List.concat |> formatPoints;
  ids = List.range 0 (List.len stations - 1);
  names = List.map (fn i -> "{name}_{i}") ids;
  _ = if List.len coords == 8 * List.len ids then Unit else error "incomplete thread coordinates";
  ribs = List.map (fn i -> start = i * 8; "{name}_{i} = " + Str.join " ⇓ " (List.take 8 (List.drop start coords))) ids;
  ribs + [name + " = " + Str.join " ⊕ " names].
header = [
  "// Revision 2: screw-on base; captive globe and strengthened gimbal.",
  "// All printable shapes, including coarse threads, are CoScad solids.",
  "radius = 32",
  "center_z = 64",
  "pin_r = 2.5",
  "bore_r = 3.1",
  "point = χ -0.005 (ψ -0.005 (ζ -0.005 (▬ 0.01 0.01 0.01)))"].
body geography = [
  "male_core = ◎ 10 12",
  "male_envelope = ((◎ 12.1 9) ⊕ ((ζ 9 (◎ 12.1 0.01)) ⇓ (ζ 12 (◎ 9.1 0.01))))",
  "male = (male_core ⊕ male_rib) ∩ male_envelope",
  "base_disk = (◎ 45 3) ⇓ (ζ 6.99 (◎ 42 0.01))",
  "base = (ζ 6.5 (◎ 10 1)) ⊕ base_disk ⊕ (ζ 6.8 (◎ 18 0.2)) ⊕ (ζ 7 male)",
  "// Socket has an entry chamfer and a 45-degree conical roof, not a bridge.",
  "socket_core = ζ -0.2 (◎ 10.4 13.2)",
  "socket_rib = female_rib ∩ (ζ -0.2 (◎ 13 13.2))",
  "socket_entry = (ζ -0.1 (◎ 13.2 0.01)) ⇓ (ζ 2.8 (◎ 10.4 0.01))",
  "socket_roof = (ζ 12.99 (◎ 12.4 0.01)) ⇓ (ζ 25.4 (◎ 0.01 0.01))",
  "socket = socket_core ⊕ socket_rib ⊕ socket_entry ⊕ socket_roof",
  "foot = ((◎ 18 12) ⊕ ((ζ 11.9 (◎ 18 0.1)) ⇓ (ζ 28.9 (◎ 9 0.1)))) ⊖ socket",
  "// Ring cross section grows from 3.5 x 6 to 5.5 x 10 mm.",
  "meridian = ζ center_z (θ 90 (ζ -5 ((◎ 41.5 10) ⊖ (ζ -0.1 (◎ 36 10.2)))))",
  "shaft = ζ 26 (◎ pin_r 78)",
  "lower_bearing = ζ 28 (◎ 4.5 6.4)",
  "upper_bearing = (ζ 96.6 (◎ 2.5 0.1)) ⇓ (ζ 98.6 (◎ 4.5 3.5))",
  "gimbal = foot ⊕ meridian ⊕ shaft ⊕ lower_bearing ⊕ upper_bearing",
  geography,
  "// Block support in both mating threads and the complete moving bore.",
  "bearing_blocker = ζ 30.5 (◎ 5.5 69)",
  "socket_blocker = ◎ 13.3 25.5",
  "support_blocker = bearing_blocker ⊕ socket_blocker",
  "coupon_female = (◎ 18 12) ⊖ socket"].
views = [
  ("gimbal","gimbal"),
  ("rotor","rotor"),
  ("base","base"),
  ("support-blocker","support_blocker"),
  ("assembly","(ζ 7 (gimbal ⊕ rotor)) ⊕ base"),
  ("layout","(χ -50 (gimbal ⊕ rotor)) ⊕ (χ 50 base)"),
  ("coupon-female","coupon_female"),
  ("coupon-male","(◎ 18 2) ⊕ (ζ 2 male)")].
# Each view is compiled from the corresponding main expression. Only the
# assembly source is part of the published set; the others are intermediates.
number n = J.JNum n.
design digest =
  thread = J.JObj [("hand",J.JStr "right"),("major_diameter_mm",number 24),("nominal_core_diameter_mm",number 20),
    ("pitch_mm",number pitch),("engagement_mm",number 12),("female_radial_enlargement_mm",number 0.4),
    ("female_axial_flank_enlargement_mm",number 0.25),("segments_per_turn",number perTurn),("male_lead_in_mm",number 3)];
  centers = J.JObj [("gimbal",J.nums [78,128]),("base",J.nums [178,128])];
  value = J.JObj [("revision",number 2),("sphere_diameter_mm",number 64),("relief_mm",number 0.8),
    ("base_diameter_mm",number 90),("assembled_height_mm",number 112.5),("gimbal_ring_radial_thickness_mm",number 5.5),
    ("gimbal_ring_depth_mm",number 10),("foot_diameter_mm",number 36),("thread",thread),("globe_center_z_print_mm",number 64),
    ("assembly_seat_z_mm",number 7),("bearing_blocker_z_mm",J.nums [30.5,99.5]),("bearing_blocker_radius_mm",number 5.5),
    ("rotor_translation_from_original_mm",number 10),("support",J.JStr "automatic tree; build plate only; socket and bearing blocked"),
    ("layout_centers_mm",centers),("original_source_sha256",J.JStr digest),
    ("physical_fit",J.JStr "Unprinted revision; printed thread coupon recommended first")];
  # Preserve the two floating whole numbers used by the original design manifest.
  J.pretty value |> Str.replace "\"pitch_mm\": 6," "\"pitch_mm\": 6.0,"
                 |> Str.replace "\"segments_per_turn\": 48," "\"segments_per_turn\": 48.0,".
generate dir compiler =
  original = readPath "{dir}/../globe.coscad" |> Str.replace "\r\n" "\n" |> Str.replace "\r" "\n";
  start = Str.indexOf "ocean =" original;
  end = Str.indexOf "// Support exclusion" original;
  _ = if start < 0 or end < start then error "missing source globe relief" else Unit;
  geography = Str.slice original start end |> Str.lines |> List.filter (fn line -> not (Str.startsWith "point =" line)) |> Str.join "\n";
  lines = header + helix "male_rib" 0 0 + helix "female_rib" 0.4 0.25 + body geography;
  source = Str.join "\n" lines + "\n";
  tmp = tempDir "globe-exports";
  _ = List.map (W.exportView dir tmp compiler source "assembly") views;
  hash = Hex.encode (Sha256.bytes (unwrap (Utf8.encode original)));
  _ = writePath "{dir}/design.json" (design hash + "\n");
  print "Generated threaded base, captive assembly, support blockers and fit coupons".
> argv = args Unit;
  usage = "usage: generate.sol [--coscad PATH]";
  if List.has "--help" argv or List.has "-h" argv then print usage else
    opts = unwrap (C.parse [C.Option "coscad" "" C.One] argv);
    rel = "models/print-in-place-globe/v2-threaded";
    dir = W.query (P.spec ["realpath",W.env "COSCAD_GLOBE_DIR" (if exists "{rel}/generate.sol" then rel else ".")]);
    preferred = C.valueOr (W.env "COSCAD" "") "coscad" opts;
    if C.operands opts != [] then error usage else
      generate dir (if preferred == "" then W.query (P.inDir "{dir}/../../.." (P.spec ["cabal","list-bin","exe:coscad"])) else preferred).
