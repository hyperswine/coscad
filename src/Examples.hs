module Examples where

import Lib (Shape (Diff, Extrude, Rz, Shape2D, Tx, Ty, Union), gen)

-- Gear generation using functional programming
-- Parameters: number of teeth, gear radius, inner radius, thickness
gear numTeeth gearRadius innerRadius thickness =
  let -- Base gear body (extruded circle)
      c1 = Shape2D 100 gearRadius -- Circle profile
      s1 = Extrude thickness c1

      -- Inner hole (extruded circle)
      c2 = Shape2D 100 innerRadius -- Circle profile
      s2 = Extrude thickness c2

      -- Base gear with hole
      baseGear = Diff s1 s2

      -- Single tooth (extruded triangle)
      toothProfile = Shape2D 3 2 -- Triangle profile
      tooth = Extrude thickness toothProfile

      -- Calculate angle between teeth
      angleStep = 360.0 / fromIntegral numTeeth

      -- Generate list of angles for each tooth
      angles = map (* angleStep) [0.0 .. fromIntegral numTeeth - 1]

      -- Position a tooth at a given angle
      positionTooth angle =
        let -- Convert angle to radians
            radians = angle * pi / 180.0
            -- Calculate position on circumference
            x = gearRadius * cos radians
            y = gearRadius * sin radians
            -- Translate tooth to position
            positioned = Tx x (Ty y tooth)
            -- Rotate tooth to face outward
            rotated = Rz angle positioned
         in rotated -- Generate all teeth using map
      teeth = map positionTooth angles

      -- Combine all teeth using fold
      allTeeth = foldl (\acc t -> Union [acc, t]) tooth teeth

      -- Final gear with teeth
      finalGear = Union [baseGear, allTeeth]
   in finalGear

-- Example gears
gear8 = gear 8 10 5 2 -- 8-tooth gear

gear12 = gear 12 15 7 2 -- 12-tooth gear

gear16 = gear 16 20 8 2 -- 16-tooth gear

-- Positioned gears for display
g1 = Tx (-40) gear8

g2 = Tx 0 gear12

g3 = Tx 50 gear16

-- Combined display
gearDemo = Union [g1, g2, g3]

--- >>> gen gearDemo
