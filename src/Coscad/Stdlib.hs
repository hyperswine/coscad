-- | Printable connection recipes expanded to ordinary CoScad solids. No
-- backend-specific geometry or imports; radii and clearances are in mm.
module Coscad.Stdlib
  ( ThreadSpec(..), tesseractThread, threadMale, threadFemale
  , globeStud, globeSocket, clearancePin, clearanceBore, captiveBearing
  , standardNames, standardShape
  ) where

import Control.Monad (unless)
import Coscad.Shape

-- | Full axial widths at root and crest; clearance is never global scaling.
data ThreadSpec = ThreadSpec
  { rootRadius :: Double, majorRadius :: Double, threadPitch :: Double
  , threadLength :: Double, rootWidth :: Double, crestWidth :: Double
  , facetsPerTurn :: Int
  } deriving (Show)

tesseractThread :: ThreadSpec
tesseractThread = ThreadSpec 4.05 5 3 8 2.2 0.2 24

need :: Bool -> String -> Either String ()
need ok why = unless ok (Left why)
finite :: [Double] -> Bool
finite = all (\x -> not (isNaN x || isInfinite x) && abs x <= 100000)
positive :: [Double] -> Bool
positive xs = finite xs && all (>0) xs

checkThread :: ThreadSpec -> Double -> Double -> Either String ()
checkThread s rc ac = do
  need (positive [r,m,p,l,w,c] && r > 0.4 && l > 0.8)
    "thread: finite positive dimensions required; root radius > 0.4 and length > 0.8 mm"
  need (finite [rc,ac] && rc >= 0 && ac >= 0) "thread: radial and axial clearances must be finite and nonnegative"
  need (m > r && w > c && c > 0.04 && w + 2*ac < p)
    "thread: major radius > root radius, root width > crest width > 0.04, and cleared root width < pitch required"
  need (n >= 12 && n <= 192) "thread: facets per turn must be between 12 and 192"
  need ((l / p + 3) * fromIntegral n <= 4096) "thread: geometry budget exceeds 4096 facets; reduce length/resolution or increase pitch"
  where ThreadSpec r m p l w c n = s

-- Tesseract-compatible 0.04 mm section recipe, phase z=pitch*angle/360.
ridge :: ThreadSpec -> Double -> Double -> Shape
ridge (ThreadSpec r m p l w c n) rc ac = Union
  [ Tz (fromIntegral i*p/fromIntegral n) (Rz (fromIntegral i*step) facet)
  | i <- [-turns*n .. n-1] ]
  where
    turns = ceiling ((l+0.3)/p) :: Int
    step = 360/fromIntegral n
    section = Hull [Tx (r-0.03+rc) (Cuboid (0.04,0.04,w+2*ac) 0 0),
                    Tx (m+rc) (Cuboid (0.04,0.04,c+2*ac) 0 0)]
    facet = Hull [section, Tz (p/fromIntegral n) (Rz step section)]

-- | Top anchored, z=-length..0, RH, angle=0 phase at z=0.
threadMale :: ThreadSpec -> Either String Shape
threadMale s = do
  checkThread s 0 0
  let ThreadSpec r m _ l _ _ _ = s
  pure (Union [Tz (-l/2) (Cyl r l 0.4 0),
               Intersection [ridge s 0 0, Tz (-(l-0.6)/2) (ZCyl (m+0.1) (l-0.6))]])

-- | Subtract this cutter from a host; includes 0.3 mm end allowance and mouth.
threadFemale :: ThreadSpec -> Double -> Double -> Either String Shape
threadFemale s rc ac = do
  checkThread s rc ac
  let ThreadSpec r m _ l _ _ _ = s
      sleeve radius = Tz (-l/2) (ZCyl radius (l+0.6))
  pure (Union [sleeve (r+rc), Intersection [ridge s rc ac,sleeve (m+rc+0.1)], ZCyl (m+rc+0.2) 0.8])

-- The globe's distinct point-hull thread profile, preserving its successful
-- end taper and bridge-free socket roof, rather than substituting a new thread.
globeRidge :: Double -> Double -> Double -> Double -> Double -> Double -> Shape
globeRidge r m p l rc ac = Union [Hull (concatMap corners [i,i+1]) | i <- [-48 .. turns*48-1]]
  where
    turns = ceiling (l/p) + 1 :: Int
    corners i = [Translate ((radius+rc)*cos a,(radius+rc)*sin a,p*fromIntegral i/48+dz) (Cuboid (0.01,0.01,0.01) 0 0)
                | (radius,dz) <- [(r-0.05,-(m-r+0.2)-ac),(m,-0.2-ac),(m,0.2+ac),(r-0.05,m-r+0.2+ac)]]
      where a = fromIntegral i * 2*pi/48

checkGlobe :: Double -> Double -> Double -> Double -> Double -> Double -> Either String ()
checkGlobe r m p l rc ac = do
  need (positive [r,m,p,l] && r > 0.9 && m > r && l > 3) "globe thread: positive root < major radii, pitch and length > 3 mm required"
  need (finite [rc,ac] && rc >= 0 && ac >= 0) "globe thread: clearances must be finite and nonnegative"
  need (2*(m-r+0.2+ac) < p) "globe thread: cleared profile must be narrower than pitch"
  need ((l/p+3)*48 <= 4096) "globe thread: geometry budget exceeds 4096 facets"

globeStud :: Double -> Double -> Double -> Double -> Either String Shape
globeStud r m p l = do
  checkGlobe r m p l 0 0
  pure (Intersection [Union [Cylinder r l, globeRidge r m p l 0 0],
    Union [Cylinder (m+0.1) (l-3), Hull [Tz (l-3) (Cylinder (m+0.1) 0.01),Tz l (Cylinder (r-0.9) 0.01)]]])

globeSocket :: Double -> Double -> Double -> Double -> Double -> Double -> Either String Shape
globeSocket r m p l rc ac = do
  checkGlobe r m p l rc ac
  need (r > 0.9) "globe thread: root radius must exceed 0.9 mm"
  pure (Union [Tz (-0.2) (Cylinder (r+rc) (l+1.2)),
    Intersection [globeRidge r m p l rc ac,Tz (-0.2) (Cylinder (m+rc+0.6) (l+1.2))],
    Hull [Tz (-0.1) (Cylinder (m+rc+0.8) 0.01),Tz 2.8 (Cylinder (r+rc) 0.01)],
    Hull [Tz (l+0.99) (Cylinder (m+rc) 0.01),Tz (l+1+m+rc) (Cylinder 0.01 0.01)]])

-- | Bottom anchored pin and positive bore cutter; no assumed printer fit.
clearancePin :: Double -> Double -> Double -> Double -> Either String Shape
clearancePin r l fr fh = do
  need (positive [r,l,fr,fh] && fr > r) "pin: positive dimensions and flange radius > shaft radius required"
  pure (Union [Cylinder fr fh,Cylinder r (l+fh)])
clearanceBore :: Double -> Double -> Double -> Either String Shape
clearanceBore r l gap = do
  need (positive [r,l] && finite [gap] && gap >= 0) "bore: positive radius/length and nonnegative finite radial clearance required"
  pure (Cylinder (r+gap) l)

-- | Axle with two stops and one separated captive sleeve. Two printed solids.
-- Positive gaps are required; this is geometry, not a support-free guarantee.
captiveBearing :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Either String Shape
captiveBearing r spanLen stopR stopH radial axial wall = do
  need (positive [r,spanLen,stopR,stopH,radial,axial,wall] && spanLen > 2*axial && stopR > r+radial+wall)
    "bearing: positive dimensions/gaps, span > twice axial gap, stops wider than sleeve required"
  let bore = r+radial; outer = bore+wall
      sleeve = Tz (stopH+axial) (Diff (Cylinder outer (spanLen-2*axial)) (Tz (-0.1) (Cylinder bore (spanLen-2*axial+0.2))))
  pure (Union [Cylinder stopR stopH,Tz stopH (Cylinder r spanLen),Tz (stopH+spanLen) (Cylinder stopR stopH),sleeve])

standardNames :: [String]
standardNames = ["thread_male","thread_female","globe_stud","globe_socket","clearance_pin","clearance_bore","captive_bearing"]

standardShape :: String -> [Double] -> Either String Shape
standardShape name args = case (name,args) of
  ("thread_male",[r,m,p,l,w,c,n]) -> spec r m p l w c n >>= threadMale
  ("thread_female",[r,m,p,l,w,c,n,rc,ac]) -> spec r m p l w c n >>= \s -> threadFemale s rc ac
  ("globe_stud",[r,m,p,l]) -> globeStud r m p l
  ("globe_socket",[r,m,p,l,rc,ac]) -> globeSocket r m p l rc ac
  ("clearance_pin",[r,l,fr,fh]) -> clearancePin r l fr fh
  ("clearance_bore",[r,l,g]) -> clearanceBore r l g
  ("captive_bearing",[r,l,s,h,rg,ag,w]) -> captiveBearing r l s h rg ag w
  _ -> Left ("invalid standard-library constructor/arity: " ++ name)
  where
    spec r m p l w c n = do
      need (finite [n] && n >= 12 && n <= 192 && n == fromIntegral (round n :: Int)) "thread: facets per turn must be a whole number between 12 and 192"
      pure (ThreadSpec r m p l w c (round n))
