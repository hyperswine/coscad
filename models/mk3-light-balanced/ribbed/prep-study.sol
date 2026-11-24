M = use "../../bolted-glider-light/slice-study-common".
N = use "../../lib/num".
main : unsafe Unit -> Unit .
main Unit = case args Unit of [template,output] ->
  pts = M.stlPoints "glider.stl";
  (unique,ids) = M.dedupe pts;
  (lx,hx,ly,hy,lz) = List.fold (fn b p -> (a,c,d,e,f)=b; (x,y,z)=p; (Numeric.min a x,Numeric.max c x,Numeric.min d y,Numeric.max e y,Numeric.min f z)) (1e9,0 - 1e9,1e9,0 - 1e9,1e9) unique;
  verts = List.map (fn p -> (x,y,z)=p; "<vertex x=\"" + N.fixed 8 (x - Num.div (lx+hx) 2) + "\" y=\"" + N.fixed 8 (y - Num.div (ly+hy) 2) + "\" z=\"" + N.fixed 8 (z - lz) + "\"/>") unique;
  mesh = "<mesh><vertices>" + Str.join "" verts + "</vertices><triangles>" + Str.join "" (M.triangles ids pts) + "</triangles></mesh>";
  writePath output (M.replaceMeshes mesh (readPath template))
  | _ -> error "usage: sol prep-study.sol TEMPLATE_XML OUTPUT_XML".
> main Unit.
