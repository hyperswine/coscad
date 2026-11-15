# Grid search over limb designs at a fixed peak stress. Writes search-SIGMA.json.
#
#   "$SOL" models/pla-bow-launcher/search.sol [SIGMA_MPa]      (default 30)
#
# The 243 designs run in 8 worker processes (this script with --worker K/8),
# as the original did with a process pool. Workers re-run the same file, so
# SOL must name the Sol executable if `sol` is not on PATH.
S = use "sim".
N = use "../lib/num".
R = use "../lib/root".
P = use "sol/lib/proc".

workers = 8.
lens = [210, 230, 250].
thicknesses = [2.4, 3.0, 3.6].
roots = [28, 36, 44].
kappas = [0.0, 0.001, 0.002].
slacks = [5, 10, 20].
# itertools.product order: L, t, b0, kappa0, slack (last varies fastest)
grid = List.concat (List.map (fn l -> List.concat (List.map (fn t -> List.concat (List.map (fn b -> List.concat (List.map (fn k ->
         List.map (fn sl -> (l, t, b, k, sl)) slacks) kappas)) roots)) thicknesses)) lens).

params row = (l, t, b, k, sl) = row;
  [("L", S.VI l), ("t", S.VF t), ("b0", S.VI b), ("bt", S.VF 8.0), ("S", S.VI (20 + l - sl)), ("R0", S.VF 20.0), ("kappa0", S.VF k)].
cols = ["L", "t", "b0", "kappa0", "S", "limb_mass_g", "brace_mm", "draw_mm", "power_stroke_mm", "stored_J",
        "brace_stored_J", "peak_draw_force_N", "v_glider_90pct", "glider_share_of_energy"].
# Python's str() of a value in a row
pyStr v = case v of
    S.VI i -> str i
  | S.VF x -> N.repr x
  | S.VNaN -> "nan"
  | S.VS s -> s
  | S.VL items -> "[...]".
field name fields = List.fold (fn acc kv -> (k, v) = kv; if k == name then v else acc) S.VNaN fields.
num v = case v of S.VF x -> x | S.VI i -> i | _ -> 0 - 1.
isNaN v = case v of S.VNaN -> True | _ -> False.

# One design: (index, ok, rendered row object, column strings, v90, limb mass)
one sigma i row =
  (l, t, b, k, sl) = row;
  head = params row;
  case S.simulate l t b 8.0 (20 + l - sl) k sigma 10.0 8.0 20.0 False of
    Err why -> (i, False, [("L", S.VI l), ("t", S.VF t), ("b0", S.VI b), ("kappa0", S.VF k), ("S", S.VI (20 + l - sl)), ("error", S.VS why)])
  | Ok res -> (fields, curve) = res;
      all = head + fields;
      ok = not (isNaN (field "v_glider_90pct" all));
      # the original drops the stress profile from solved rows only
      kept = if ok then List.filter (fn kv -> (kk, v) = kv; kk != "sigma_profile_full_draw") all else all;
      (i, ok, kept).

# ---- worker: print its share of the grid, one block per design ----------------
worker : unsafe Int -> Int -> Int -> Unit.
worker sigma kth total =
  mine = List.filter (fn q -> (i, row) = q; i % total == kth) (List.zip (List.range 0 (List.len grid - 1)) grid);
  _ = List.map (fn q -> (i, row) = q; (j, ok, fields) = one sigma i row;
        okText = if ok then "1" else "0";
        summary = Str.join "\t" (List.map (fn c -> pyStr (field c fields)) cols);
        print ("@@ROW\t{j}\t{okText}\t{N.repr (num (field "v_glider_90pct" fields))}\t{N.repr (num (field "limb_mass_g" fields))}\t"
               + summary + "\n " + S.renderObj 1 fields + "\n@@END")) mine;
  Unit.

# ---- parent ------------------------------------------------------------------
startWorker sol script sigma kth =
  spec = P.spec [sol, script, "--worker", "{kth}/{workers}", N.repr sigma];
  case Proc.start (PipelineSpec [spec] (PipeBytes (Bytes.empty Unit)) (PipeCapture 4194304) PipeOutputInherit 0 PipeFail) of
    Ok job -> job
  | Err why -> error (str why).
collectJob job = case Proc.wait job of
    Ok (PipelineResult 0 _ out _) -> unwrap (Utf8.decode out)
  | Ok (PipelineResult code _ _ _) -> error "search worker exited {code}"
  | Err why -> error (str why).
# parse "@@ROW ... @@END" blocks into (index, ok, v90, mass, summary, json text)
blocks text =
  (rows, cur) = List.fold (fn st line -> (acc, open) = st;
      if Str.startsWith "@@ROW\t" line then (acc, [line])
      else if line == "@@END" then (List.rev open :: acc, [])
      else if open == [] then st else (acc, line :: open)) ([], []) (Str.lines text);
  List.map (fn b -> hdr = Str.split 9 (b ! 0);
      (unwrap (Try.parseInt (hdr ! 1)), (hdr ! 2) == "1", unwrap (Try.parseNum (hdr ! 3)), unwrap (Try.parseNum (hdr ! 4)),
       Str.join " " (List.drop 5 hdr), Str.join "\n" (List.drop 1 b))) rows.
# stable insertion: equal keys keep their order
insertBy key x acc = List.filter (fn e -> key e <= key x) acc + [x] + List.filter (fn e -> key e > key x) acc.
sortBy key xs = List.fold (fn acc x -> insertBy key x acc) [] xs.

main : unsafe Unit -> Unit.
main u =
  argv = args Unit;
  case argv of
    ["--worker", spec, sig] -> parts = Str.split 47 spec;
      worker (unwrap (Try.parseNum sig)) (unwrap (Try.parseInt (parts ! 0))) (unwrap (Try.parseInt (parts ! 1)))
  | _ ->
    sigma = case argv of [s] -> unwrap (Try.parseNum s) | [] -> 30.0 | _ -> error "usage: search.sol [SIGMA_MPa]";
    dir = R.modelDir "models/pla-bow-launcher" "search.sol";
    sol = case unwrap (Env.get "SOL") of EnvText p -> p | EnvMissing -> "sol";
    jobs = List.map (startWorker sol "{dir}/search.sol" sigma) (List.range 0 (workers - 1));
    rows = List.concat (List.map (fn j -> blocks (collectJob j)) jobs) |> sortBy (fn r -> (i, ok, v, m, sm, js) = r; i);
    _ = if List.len rows == List.len grid then Unit else error "workers returned {List.len rows} of {List.len grid} designs";
    json = "[\n" + Str.join ",\n" (List.map (fn r -> (i, ok, v, m, sm, js) = r; js) rows) + "\n]";
    ok = List.filter (fn r -> (i, okr, v, m, sm, js) = r; okr) rows;
    _ = writePath "{dir}/search-{Num.floor sigma}.json" json;
    _ = print "{List.len ok}/{List.len rows} solved at {N.repr sigma} MPa";
    _ = print (Str.join " " cols);
    best = sortBy (fn r -> (i, okr, v, m, sm, js) = r; 0 - v) ok;
    _ = List.map (fn r -> (i, okr, v, m, sm, js) = r; print sm) (List.take 12 best);
    hit = sortBy (fn r -> (i, okr, v, m, sm, js) = r; m) (List.filter (fn r -> (i, okr, v, m, sm, js) = r; v >= 8.0) best);
    _ = print "\n{List.len hit} designs reach 8 m/s (90% energy); lightest:";
    _ = List.map (fn r -> (i, okr, v, m, sm, js) = r; print sm) (List.take 8 hit);
    Unit.
> main Unit.
