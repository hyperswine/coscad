-- Test script to demonstrate the Examples.hs gear functions
-- This shows how the functional programming approach creates parametric designs

import Examples
import Lib

-- Test the gear functions directly
main = do
  -- Generate different gears
  let smallGear = gear 6 8 4 2    -- 6-tooth gear
  let mediumGear = gear 12 15 7 2  -- 12-tooth gear
  let largeGear = gear 20 25 10 2  -- 20-tooth gear

  -- Position them
  let g1 = Tx (-30) smallGear
  let g2 = Tx 0 mediumGear
  let g3 = Tx 40 largeGear

  -- Create final scene
  let gearSet = Union [g1, g2, g3]

  -- Output to file
  writeScad gearSet "gear_demo.scad"
  putStrLn "Generated gear_demo.scad with 3 parametric gears!"

  -- Show how easy it is to create variations
  let tinyGear = gear 4 5 2 1      -- 4-tooth tiny gear
  let massiveGear = gear 36 50 20 3 -- 36-tooth massive gear

  writeScad tinyGear "tiny_gear.scad"
  writeScad massiveGear "massive_gear.scad"
  putStrLn "Generated additional gear variations!"
