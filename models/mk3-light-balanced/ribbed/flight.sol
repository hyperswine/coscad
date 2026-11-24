# Model-only relative extrusion. Input is a G-code file with its model centered
# on (128,128), original STL diagonal rotation retained and Z minimum on bed.
F = use "../../lib/glider-flight".
N = use "../../lib/num".
J = use "sol/lib/json".
num = J.JNum.
s = 1.035.
area = 110*85*s*s/1e6.
mac = 170*s/3.0.
wingAC = 39.5*s.
tailAC = 101*s.
factor = 0.26*0.8*0.8*0.65.
neutral = (wingAC + factor*tailAC)/(1+factor).
scenario mass fraction =
  m = mass*fraction;
  speed = Num.sqrt (2*m*0.00980665/(1.225*area*0.6));
  J.JObj [("fraction_of_PLA_mass",num fraction),("mass_g",num m),("loading_g_dm2",num (m/(area*100))),("speed_at_CL_0.6_m_s",num speed),("Re_at_MAC",num (speed*mac/1000.0/0.000015))].
> case args Unit of [mesh,file] ->
    (lx,hx,ly,hy,_) = List.fold F.bounds (1e9,0 - 1e9,1e9,0 - 1e9,1e9) (Str.lines (readPath mesh));
    (_,_,_,_,e,sx,sy,_) = List.fold F.advance (0,0,0,False,0,0,0,0) (Str.lines (readPath file));
    x = Num.div sx e - 128 + Num.div (lx+hx) 2;
    y = Num.div sy e - 128 + Num.div (ly+hy) 2;
    cg = Num.div (y - x) (Num.sqrt 2);
    mass = e*N.pi*0.875*0.875*1.26/1000.0;
    cl = 0.6*(cg - wingAC)/(tailAC - wingAC);
    print (J.pretty (J.JObj [("area_dm2",num (area*100)),("cg_aft_of_nose_mm",num (cg+130.5*s)),("neutral_aft_of_nose_mm",num (neutral+130.5*s)),("static_margin_percent",num (100*(neutral - cg)/mac)),("trim_tail_incidence_deg_at_CL_0.6",num (cl/(0.8*0.26*0.06) - 0.65*(0.6 - cl)/0.075)),("scenarios",J.JArr (List.map (scenario mass) [1,0.75,0.60,0.50]))]))
  | _ -> error "usage: sol flight.sol MESH GCODE".
