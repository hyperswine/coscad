# Build and verify both threaded globe projects and their offline slices.
C = use "sol/lib/cli".
P = use "sol/lib/proc".
W = use "../workflow".
parts = [[("gimbal","gimbal"),("rotor","rotor"),("base","base")],
         [("support-blocker","support_blocker"),("coupon-female","coupon_female"),("coupon-male","(◎ 18 2) ⊕ (ζ 2 male)")]].
slices = [("slice","process.json","globe-project.3mf","globe-v2-p1s.gcode.3mf"),
          ("coupon-slice","process-coupon.json","thread-coupon-project.3mf","thread-coupon-p1s.gcode.3mf")].
build : unsafe String -> C.Parsed -> Unit.
build dir opts =
  python = C.valueOr (W.env "COSCAD_GLOBE_PYTHON" "python3") "python" opts;
  mpl = W.env "MPLCONFIGDIR" "/private/tmp/globe-matplotlib";
  script name argv = W.command (P.withEnv "MPLCONFIGDIR" mpl (P.spec ([python,"{dir}/{name}.py"] + argv)));
  chosen = C.valueOr (W.env "COSCAD" "") "coscad" opts;
  compiler = if chosen != "" then chosen else W.query (P.inDir "{dir}/../../.." (P.spec ["cabal","list-bin","exe:coscad"]));
  _ = W.command (P.spec [W.env "SOL" "sol","{dir}/generate.sol","--coscad",compiler] |> P.withEnv "COSCAD_GLOBE_DIR" dir);
  prefix = W.prefixOf "{dir}/assembly.coscad";
  tmp = tempDir "globe-v2-parts";
  _ = W.renderParts dir tmp compiler prefix parts;
  original = "{tmp}/original-rotor.coscad";
  reference = "{tmp}/original-rotor.stl";
  _ = W.partSource original (W.prefixOf "{dir}/../globe.coscad") "rotor";
  _ = W.command (P.spec [compiler,"stl",original,"-o",reference]);
  _ = script "verify" ["--reference-rotor",reference];
  if C.flag "render-only" opts then Unit else
    _ = script "prepare_slice" [];
    _ = script "prepare_coupon" [];
    slicer = C.valueOr "/Applications/BambuStudio.app/Contents/MacOS/BambuStudio" "slicer" opts;
    _ = List.map (W.sliceProject dir slicer) slices;
    _ = List.map (fn name -> script name []) ["verify_toolpaths","finalize_package","finalize_coupon"];
    Unit.
> argv = args Unit;
  usage = "usage: build.sol [--render-only] [--coscad PATH] [--python PATH] [--slicer PATH]";
  if List.has "--help" argv or List.has "-h" argv then print usage else
    opts = unwrap (C.parse [C.Option "coscad" "" C.One,C.Option "python" "" C.One,C.Option "slicer" "" C.One,C.Option "render-only" "" C.Switch] argv);
    rel = "models/print-in-place-globe/v2-threaded";
    dir = W.env "COSCAD_GLOBE_DIR" (if exists "{rel}/build.sol" then rel else ".");
    if C.operands opts != [] or not (exists "{dir}/generate.sol") then error usage
    else build (W.query (P.spec ["realpath",dir])) opts.
