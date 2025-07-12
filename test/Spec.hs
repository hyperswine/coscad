{-
  Tests are unnecessary due to use of types.
  This isnt java, but haskell
-}

import Lib

main :: IO ()
main = do
  putStrLn "Testing examples ..."
  writeScad example1 "output/example1.scad"

-- | Two cubes, one at the origin and one translated +2 on the X axis.
example1 = (■) 1 ⊕ (■) 1 ▷ χ 2

-- >>> (gen example1)
-- "union() {\n  cube([1.0, 1.0, 1.0]);\n  translate([2.0, 0, 0]) {\n    cube([1.0, 1.0, 1.0]);\n  }\n}"

-- >>> writeScad example1 "output/example1.scad"
