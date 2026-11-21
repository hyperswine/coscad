J = use "sol/lib/json".
R = use "../lib/root".
number j key = unwrap (J.get key j |>? J.num).
speed mass = Num.sqrt (2 * mass * 0.00980665 / (1.225 * 0.05 * 0.6)).
> dir = R.modelDir "models/bolted-glider-retrim" "flight-screen.sol";
  reference = unwrap (J.parse (readPath "{dir}/../bolted-glider/balance.json"));
  mac = Num.div (number reference "target_cg_y") (number reference "target_cg_fraction_mac");
  parts = [(45,74),(45,74),(57.7,51.7),(15.5,195),(6.7,195),(15.2,74),(8.8,184)];
  (mass,moment) = List.fold (fn acc part -> (m,q) = acc; (w,y) = part; (m+w,q+w*y)) (0,0) parts;
  cg = Num.div moment mass; initial = mac * number reference "neutral_point_fraction_mac";
  tail = Num.div (initial - mac/4) (184 - mac/4); neutral = initial + 24*(1 - tail);
  print (J.pretty (J.JObj [("assumed_mass_g",J.JNum mass),("cg_y_mm",J.JNum cg),("neutral_point_y_mm",J.JNum neutral),
    ("static_margin_percent",J.JNum (100*(neutral - cg)/mac)),("target_cg_y_mm",J.JArr [J.JNum (neutral - 0.15*mac),J.JNum (neutral - 0.10*mac)]),
    ("speed_m_s_at_CL_0.6",J.JNum (speed mass)),("reference_speed_m_s_at_CL_0.6",J.JNum (speed (number reference "flying_g")))])).
