-- | Opt-in geometry experiments. The production default remains OpenSCAD.
-- Unsupported native operations are errors, never an implicit backend switch.
module Coscad.Backend
  ( Backend (..)
  , parseBackend
  , backendName
  , renderShape
  , nativeMesh
  , nativeCSGMesh
  , nativeCSGMeshAllowEmpty
  , nativeIndexedCSGMesh
  ) where

import Control.Monad (unless)
import Data.List (nub)
import Coscad.Codegen (renderScad)
import Coscad.Geometry (resolve)
import Coscad.IO (writeFileUtf8)
import Coscad.Linalg
import Coscad.Mesh (Tri, meshMap, stlAscii, triArea)
import Coscad.Manifold
import Coscad.Mesh.Indexed
import Coscad.OpenScad (renderStlFile)
import Coscad.Shape
import Coscad.Validate (finite, validateShape)

data Backend = OpenScadBackend | NativeExperimental deriving (Eq, Show)

parseBackend :: String -> Either String Backend
parseBackend "openscad" = Right OpenScadBackend
parseBackend "native-experimental" = Right NativeExperimental
parseBackend name = Left ("unknown backend '" ++ name ++ "'; use openscad or native-experimental")

backendName :: Backend -> String
backendName OpenScadBackend = "openscad"
backendName NativeExperimental = "native-experimental"

-- | The caller owns output paths. OpenSCAD's warning/error policy is unchanged.
-- Used by single-part and manufacturing exports; images remain OpenSCAD.
renderShape :: Backend -> FilePath -> FilePath -> Shape -> IO (Either String [Tri])
renderShape OpenScadBackend scadF stlF shape = do
  writeFileUtf8 scadF (renderScad shape)
  renderStlFile scadF stlF
renderShape NativeExperimental scadF stlF shape = do
  result <- nativeCSGMesh shape
  case result of
    Left err -> return (Left err)
    Right tris -> do
      writeFileUtf8 scadF (renderScad shape)
      writeFileUtf8 stlF (stlAscii "coscad_native_experimental" tris)
      return (Right tris)

-- | Resolve attachments, evaluate actual CSG through the optional native
-- kernel, and keep empty intermediate solids (e.g. a completely removed cutter).
-- Transforming a boolean result never launches OpenSCAD.
nativeCSGMesh :: Shape -> IO (Either String [Tri])
nativeCSGMesh shape = do
  result <- nativeCSGMeshAllowEmpty shape
  return $ do
    tris <- result
    unless (not (null tris)) (Left "native-experimental: empty mesh")
    return tris

-- | Assembly isolation may legitimately hide every instance of a part. Only
-- callers that explicitly support an empty solid should use this entry point.
nativeCSGMeshAllowEmpty :: Shape -> IO (Either String [Tri])
nativeCSGMeshAllowEmpty shape = fmap indexedTriangles <$> nativeIndexedCSGMesh shape

-- | Internal/native consumers retain physical vertex identities throughout
-- CSG and transforms. Empty results are valid for isolated hidden parts.
nativeIndexedCSGMesh :: Shape -> IO (Either String IndexedMesh)
nativeIndexedCSGMesh shape = case structure shape >> validateShape shape of
  Left err -> return (Left err)
  Right () -> go (resolve shape)
  where
    structure s = do
      case s of
        Union [] -> invalid
        Intersection [] -> invalid
        Hull [] -> invalid
        Minkowski [] -> invalid
        Poly (PD [] _) -> invalid
        Loft [] -> invalid
        _ -> Right ()
      mapM_ structure (shapeChildren s)
    invalid = Left "native-experimental: empty shape/profile sequence"
    point (a,b,c) = [a,b,c]
    moved f s = fmap (>>= mapIndexed f) (go s)
    rotated r = moved (mApply r)
    flipWinding = reverseIndexed
    go s = case s of
      Extrude h profile -> extruded h profile
      Empty -> return (Right emptyIndexed)
      Hidden _ -> return (Right emptyIndexed)
      Cuboid (x,y,z) c 0 | c>0 ->
        let ix=max 0.001 (x-2*c); iy=max 0.001 (y-2*c); iz=max 0.001 (z-2*c)
        in go (Hull [Cuboid (x,iy,iz) 0 0,Cuboid (ix,y,iz) 0 0,Cuboid (ix,iy,z) 0 0])
      Union xs -> many MeshUnion emptyIndexed xs
      Diff a b -> two MeshDifference a b
      Intersection [] -> return (Left "native-experimental: empty intersection")
      Intersection (x:xs) -> go x >>= \r -> case r of Left e -> return (Left e); Right mesh -> many MeshIntersection mesh xs
      Hull xs -> many MeshHull emptyIndexed xs
      Minkowski [] -> return (Left "native-experimental: empty Minkowski sum")
      Minkowski (x:xs) -> go x >>= \r -> case r of Left e -> return (Left e); Right mesh -> many MeshMinkowski mesh xs
      Tx d x -> moved (vadd (d,0,0)) x
      Ty d x -> moved (vadd (0,d,0)) x
      Tz d x -> moved (vadd (0,0,d)) x
      Translate d x -> moved (vadd d) x
      Rx a x -> rotated (rodMat a (1,0,0)) x
      Ry a x -> rotated (rodMat a (0,1,0)) x
      Rz a x -> rotated (rodMat a (0,0,1)) x
      RotAxis _ (0,0,0) _ -> return (Left "native-experimental: rotation needs a nonzero axis")
      RotAxis a v x -> rotated (rodMat a v) x
      Transform3 r d x -> moved (vadd d . mApply r) x
      Scale v x -> fmap (if odd (length (filter (<0) (point v))) then flipWinding else id) <$> moved (vmul v) x
      Mirror n x -> fmap flipWinding <$> moved (mApply (mirrorMat n)) x
      Tag _ x -> go x
      Datum _ _ x -> go x
      Frame _ _ _ x -> go x
      _ -> return (nativeMesh s >>= weldTriangles)
    extruded h profile = case profile of
      Shape2D n r | n<=4096 -> extrudedSimple h [(r*cos a,r*sin a) | j<-[0..n-1],let a=2*pi*fromIntegral j/fromIntegral n]
      Poly (PD points []) -> extrudedSimple h (unclose points)
      Poly _ -> return (Left "native-experimental: explicit polygon paths/holes are unsupported; use a 2D difference")
      Union xs -> go (Union (map (Extrude h) xs))
      Diff a b -> go (Diff (Extrude h a) (Extrude h b))
      Intersection xs -> go (Intersection (map (Extrude h) xs))
      Hull xs -> go (Hull (map (Extrude h) xs))
      Minkowski xs -> go (Minkowski (map (Extrude (h/fromIntegral (length xs))) xs))
      Tx x p -> go (Tx x (Extrude h p))
      Ty y p -> go (Ty y (Extrude h p))
      Tz 0 p -> extruded h p
      Translate (x,y,0) p -> go (Translate (x,y,0) (Extrude h p))
      Rz angle p -> go (Rz angle (Extrude h p))
      Scale (x,y,_) p -> go (Scale (x,y,1) (Extrude h p))
      Mirror (x,y,0) p -> go (Mirror (x,y,0) (Extrude h p))
      Tag _ p -> extruded h p
      Datum _ _ p -> extruded h p
      Frame _ _ _ p -> extruded h p
      _ -> return (Left ("native-experimental: unsupported extrusion profile "++takeWhile (/=' ') (show profile)))
    -- A validated single simple extrusion has no separate coincident shells.
    -- Weld only this leaf; boolean results never pass through triangle soup.
    extrudedSimple h points = fmap (>>= weldTriangles) (extrudePolygon h points)
    unclose []=[]
    unclose points@(x:_)=case reverse points of
      y:rest | x==y && not (null rest) -> reverse rest
      _ -> points
    two op a b = do
      left <- go a
      case left of
        Left e -> return (Left e)
        Right x -> do
          right <- go b
          case right of Left e -> return (Left e); Right y -> indexedOperation op x y
    many _ acc [] = return (Right acc)
    many op acc (x:xs) = do
      r <- go x
      case r of
        Left e -> return (Left e)
        Right mesh -> do
          combined <- indexedOperation op acc mesh
          case combined of Left e -> return (Left e); Right result -> many op result xs

-- | Deliberately small surface: boxes, polygonal cylinders/cones/frusta,
-- unrounded centered variants, and affine transforms. No CSG concatenation:
-- overlapping closed meshes would not be a boolean union.
nativeMesh :: Shape -> Either String [Tri]
nativeMesh shape = do
  -- Check the supported tree before the general validator computes bounds;
  -- unsupported empty CSG/profile nodes have no meaningful native bounds.
  tris <- go shape
  validateShape shape
  unless (not (null tris)) (Left "native-experimental: empty mesh")
  unless (all valid tris) (Left "native-experimental: non-finite or degenerate triangle after transform")
  return tris
  where
    valid t@(a,b,c) = all finite (vec a ++ vec b ++ vec c) && finite (triArea t) && triArea t > 0
    vec (a,b,c) = [a,b,c]
    moved f s = meshMap f <$> go s
    rotated r = moved (mApply r)
    go s = case s of
      Rectangle x y z -> Right (boxMesh x y z)
      Cuboid dims 0 r | r>0 -> Right (roundedBoxMesh dims r)
      Cuboid (x,y,z) 0 0 -> moved (vadd (-x/2,-y/2,-z/2)) (Rectangle x y z)
      Sphere r -> Right (sphereMesh 50 r)
      Cylinder r h -> Right (radialMesh 50 r r h)
      Cone r h -> Right (radialMesh 50 r 0 h)
      Frustum h a b -> Right (radialMesh 50 a b h)
      Prism n r h -> Right (radialMesh n r r h)
      Cyl r h c 0 | c>0 -> Right (profileMesh 50 (nub [(r-c,-h/2),(r,-h/2+c),(r,h/2-c),(r-c,h/2)]))
      Cyl r h 0 rounding | rounding>0 -> Right (profileMesh 50 (roundCylinderProfile r h rounding))
      Torus major minor | major>minor -> Right (torusMesh 50 50 major minor)
      Torus _ _ -> Left "native-experimental: spindle/horn torus is unsupported"
      Cyl r h 0 0 -> moved (vadd (0,0,-h/2)) (Cylinder r h)
      Tube ro ri h -> Right (tubeMesh 50 ro ri h)
      Prismoid a b h -> Right (prismoidMesh a b h)
      Wedge (x,y,z) -> Right (wedgeMesh x y z)
      ZCyl r h -> go (Cyl r h 0 0)
      XCyl r h -> rotated (rodMat 90 (0,1,0)) (Cyl r h 0 0)
      YCyl r h -> rotated (rodMat (-90) (1,0,0)) (Cyl r h 0 0)
      Tx d x -> moved (vadd (d,0,0)) x
      Ty d x -> moved (vadd (0,d,0)) x
      Tz d x -> moved (vadd (0,0,d)) x
      Translate d x -> moved (vadd d) x
      Rx a x -> rotated (rodMat a (1,0,0)) x
      Ry a x -> rotated (rodMat a (0,1,0)) x
      Rz a x -> rotated (rodMat a (0,0,1)) x
      RotAxis _ (0,0,0) _ -> Left "native-experimental: rotation needs a nonzero axis"
      RotAxis a v x -> rotated (rodMat a v) x
      Transform3 r d x -> moved (vadd d . mApply r) x
      Scale v x -> orient (odd (length (filter (<0) (vec v)))) . meshMap (vmul v) <$> go x
      Mirror n x -> orient True . meshMap (mApply (mirrorMat n)) <$> go x
      Tag _ x -> go x
      Datum _ _ x -> go x
      Frame _ _ _ x -> go x
      _ -> Left ("native-experimental: unsupported " ++ takeWhile (/= ' ') (show s)
                 ++ "; select --backend=openscad explicitly")
    orient False = id
    orient True = map (\(a,b,c) -> (a,c,b))

boxMesh :: Double -> Double -> Double -> [Tri]
boxMesh x y z = concatMap quad
  [ ((0,0,0),(0,y,0),(x,y,0),(x,0,0))
  , ((0,0,z),(x,0,z),(x,y,z),(0,y,z))
  , ((0,0,0),(x,0,0),(x,0,z),(0,0,z))
  , ((0,y,0),(0,y,z),(x,y,z),(x,y,0))
  , ((0,0,0),(0,0,z),(0,y,z),(0,y,0))
  , ((x,0,0),(x,y,0),(x,y,z),(x,0,z))
  ]

quad :: (V3,V3,V3,V3) -> [Tri]
quad (a,b,c,d) = [(a,b,c),(a,c,d)]

radialMesh :: Int -> Double -> Double -> Double -> [Tri]
radialMesh n rb rt h = concatMap sector [0 .. n-1]
  where
    ring r z i = let a = 2*pi*fromIntegral (i `mod` n)/fromIntegral n
                 in (r*cos a, r*sin a, z)
    sector i =
      let a = ring rb 0 i; b = ring rb 0 (i+1)
          c = ring rt h (i+1); d = ring rt h i
      in (if rb > 0 then [((0,0,0),b,a)] else [])
         ++ (if rt > 0 then [((0,0,h),d,c)] else [])
         ++ (if rb == 0 then [(a,c,d)] else if rt == 0 then [(a,b,c)] else quad (a,b,c,d))

-- BOSL2 wedge: triangular profile in YZ, vertical side at -Y.
wedgeMesh :: Double -> Double -> Double -> [Tri]
wedgeMesh x y z =
  [(a,c,b),(d,e,f)] ++ concatMap quad [(a,b,e,d),(a,d,f,c),(c,f,e,b)]
  where
    a=(-x/2,-y/2,-z/2); b=(-x/2,y/2,-z/2); c=(-x/2,-y/2,z/2)
    d=(x/2,-y/2,-z/2); e=(x/2,y/2,-z/2); f=(x/2,-y/2,z/2)

prismoidMesh :: (Double,Double) -> (Double,Double) -> Double -> [Tri]
prismoidMesh (bx,by) (tx,ty) h = filter ((>0) . triArea) (concatMap quad
  [(a,d,c,b),(e,f,g,i),(a,b,f,e),(b,c,g,f),(c,d,i,g),(d,a,e,i)])
  where
    a=(-bx/2,-by/2,-h/2); b=(bx/2,-by/2,-h/2); c=(bx/2,by/2,-h/2); d=(-bx/2,by/2,-h/2)
    e=(-tx/2,-ty/2,h/2); f=(tx/2,-ty/2,h/2); g=(tx/2,ty/2,h/2); i=(-tx/2,ty/2,h/2)

tubeMesh :: Int -> Double -> Double -> Double -> [Tri]
tubeMesh n ro ri h
  | ri==0 = meshMap (vadd (0,0,-h/2)) (radialMesh n ro ro h)
  | otherwise = concatMap sector [0..n-1]
  where
    ring r z j=let a=2*pi*fromIntegral (j `mod` n)/fromIntegral n in (r*cos a,r*sin a,z)
    sector j = concatMap quad [(a,b,c,d),(e,i,g,f),(d,c,g,i),(a,e,f,b)]
      where
        a=ring ro (-h/2) j; b=ring ro (-h/2) (j+1); c=ring ro (h/2) (j+1); d=ring ro (h/2) j
        e=ring ri (-h/2) j; f=ring ri (-h/2) (j+1); g=ring ri (h/2) (j+1); i=ring ri (h/2) j

-- Latitude rings with half-step polar offsets, matching the emitter's
-- OpenSCAD sphere($fn=50) discretization. The caps are planar, not pole fans.
-- Sampling contract: openscad/src/core/primitives.cc, createGeometryOrig.
sphereMesh :: Int -> Double -> [Tri]
sphereMesh n radius =
  [((0,0,top),ring 0 j,ring 0 (j+1)) | j<-[0..n-1]]
  ++ [((0,0,bottom),ring (bands-1) (j+1),ring (bands-1) j) | j<-[0..n-1]]
  ++ concat [quad (ring k j,ring (k+1) j,ring (k+1) (j+1),ring k (j+1)) | k<-[0..bands-2],j<-[0..n-1]]
  where
    bands=(n+1) `div` 2
    latitude :: Int -> Double
    latitude k=pi*(fromIntegral k+0.5)/fromIntegral bands
    top=radius*cos (latitude 0)
    bottom=radius*cos (latitude (bands-1))
    ring k j=let phi=latitude k; theta=2*pi*fromIntegral (j `mod` n)/fromIntegral n
             in (radius*sin phi*cos theta,radius*sin phi*sin theta,radius*cos phi)

-- Closed surface of revolution for ordered (radius,z) rings. Zero-radius
-- endpoints become single apices; equal adjacent rings are removed by caller.
profileMesh :: Int -> [(Double,Double)] -> [Tri]
profileMesh n rings = case rings of
  [] -> []
  first:rest -> cap False first ++ cap True (foldl (\_ b -> b) first rest)
    ++ concat [band a b | (a,b)<-zip rings rest]
  where
    point (r,z) j=let theta=2*pi*fromIntegral (j `mod` n)/fromIntegral n in (r*cos theta,r*sin theta,z)
    cap top q@(r,z)
      | r<=0 = []
      | otherwise = [if top then ((0,0,z),a,b) else ((0,0,z),b,a) | j<-[0..n-1],let a=point q j; b=point q (j+1)]
    band q@(rb,_) p@(rt,_)=concat
      [if rb==0 then [(a,c,d)] else if rt==0 then [(a,b,c)] else quad (a,b,c,d)
      | j<-[0..n-1],let a=point q j; b=point q (j+1); c=point p (j+1); d=point p j]

-- BOSL2-compatible octahedral sampling for rounded cuboid corners: the three
-- great-circle intersections at each barycentric lattice point are averaged
-- and projected onto the sphere. See third-party/bosl2 for source attribution.
-- Exact zero coordinates at octant seams are required for indexed welding.
octaSphereMesh :: Int -> Double -> [Tri]
octaSphereMesh n r = concat
  [map (orient (sx*sy*sz<0) . mapTri (vmul (r*sx,r*sy,r*sz))) octant
  | sx<-[-1,1],sy<-[-1,1],sz<-[-1,1]]
  where
    sn i | i==0 = 0 | i==n = 1 | otherwise = sin (pi*fromIntegral i/fromIntegral (2*n))
    cs i=sn (n-i)
    at j k
      | i==0 = (0,cs k,sn k)
      | j==0 = (cs k,0,sn k)
      | k==0 = (cs j,sn j,0)
      | otherwise = vnormed (foldl vadd (0,0,0)
          [meet z1 z2 y1 y2,meet z1 z2 x1 x2,meet y1 y2 x1 x2])
      where
        i=n-j-k
        z1=(cs k,0,sn k); z2=(0,cs k,sn k)
        y1=(cs j,sn j,0); y2=(0,sn j,cs j)
        x1=(sn i,cs i,0); x2=(sn i,0,cs i)
    meet a b c d=vnormed (cross (cross a b) (cross c d))
    octant=[(at j k,at (j+1) k,at j (k+1)) | j<-[0..n-1],k<-[0..n-j-1]]
      ++ [(at (j+1) k,at (j+1) (k+1),at j (k+1)) | j<-[0..n-2],k<-[0..n-j-2]]
    mapTri f (a,b,c)=(f a,f b,f c)
    orient True (a,b,c)=(a,c,b)
    orient False tri=tri

-- Exact convex Minkowski boundary of the centred inner box and sampled
-- octahedral sphere: eight corner patches, twelve ruled edge strips and six
-- planar faces. This avoids the general kernel's transient sliver faces.
roundedBoxMesh :: V3 -> Double -> [Tri]
roundedBoxMesh (x,y,z) r = corners++edgesX++edgesY++edgesZ++facesX++facesY++facesZ
  where
    hx=max 0.001 (x-2*r)/2; hy=max 0.001 (y-2*r)/2; hz=max 0.001 (z-2*r)/2
    sign a=if a<0 then -1 else 1
    shifted tri@(a,b,c) =
      let (cx,cy,cz)=vadd a (vadd b c)
          offset=(sign cx*hx,sign cy*hy,sign cz*hz)
      in mapTri (vadd offset) tri
    mapTri f (a,b,c)=(f a,f b,f c)
    corners=map shifted (octaSphereMesh 13 r)
    sn i | i==0=0 | i==13=1 | otherwise=sin (pi*fromIntegral i/26)
    cs i=sn (13-i)
    outward hint q@(a,b,c,_) =
      let ts=quad q
      in if dot3 (cross (vsub b a) (vsub c a)) hint<0 then [(u,w,v) | (u,v,w)<-ts] else ts
    edgesX=concat [let at xx k=(xx,sy*(hy+r*cs k),sz*(hz+r*sn k))
                  in outward (0,sy,sz) (at (-hx) j,at hx j,at hx (j+1),at (-hx) (j+1))
                 | sy<-[-1,1],sz<-[-1,1],j<-[0::Int ..12]]
    edgesY=concat [let at yy k=(sx*(hx+r*cs k),yy,sz*(hz+r*sn k))
                  in outward (sx,0,sz) (at (-hy) j,at hy j,at hy (j+1),at (-hy) (j+1))
                 | sx<-[-1,1],sz<-[-1,1],j<-[0::Int ..12]]
    edgesZ=concat [let at zz k=(sx*(hx+r*cs k),sy*(hy+r*sn k),zz)
                  in outward (sx,sy,0) (at (-hz) j,at hz j,at hz (j+1),at (-hz) (j+1))
                 | sx<-[-1,1],sy<-[-1,1],j<-[0::Int ..12]]
    facesX=concat [let xx=s*(hx+r) in outward (s,0,0) ((xx,-hy,-hz),(xx,hy,-hz),(xx,hy,hz),(xx,-hy,hz)) | s<-[-1,1]]
    facesY=concat [let yy=s*(hy+r) in outward (0,s,0) ((-hx,yy,-hz),(hx,yy,-hz),(hx,yy,hz),(-hx,yy,hz)) | s<-[-1,1]]
    facesZ=concat [let zz=s*(hz+r) in outward (0,0,s) ((-hx,-hy,zz),(hx,-hy,zz),(hx,hy,zz),(-hx,hy,zz)) | s<-[-1,1]]

roundCylinderProfile :: Double -> Double -> Double -> [(Double,Double)]
roundCylinderProfile r h rounding = nub (bottom ++ top)
  where
    -- BOSL2's circum=false convention scales the axial corner radius by
    -- cos(180/$fn), preserving the requested radial inset at facet vertices.
    rz=rounding*cos (pi/50)
    sn i | i==0 = 0 | i==13 = 1 | otherwise=sin (pi*fromIntegral i/26)
    cs i=sn (13-i)
    bottom=[(r-rounding+rounding*sn i,-h/2+rz*(1-cs i)) | i<-[0::Int ..13]]
    top=[(r-rounding+rounding*cs i,h/2-rz*(1-sn i)) | i<-[0::Int ..13]]

torusMesh :: Int -> Int -> Double -> Double -> [Tri]
torusMesh around section major minor = concat
  [quad (point i j,point (i+1) j,point (i+1) (j+1),point i (j+1))
  | i<-[0..around-1],j<-[0..section-1]]
  where
    point i j=let a=2*pi*fromIntegral (i `mod` around)/fromIntegral around
                  b=2*pi*fromIntegral (j `mod` section)/fromIntegral section
                  radius=major+minor*cos b
              in (radius*cos a,radius*sin a,minor*sin b)
