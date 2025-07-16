module Examples where

import Lib (D, PD (..), Shape (Diff, Extrude, Poly, Rz, Shape2D, Tx, Ty, Union), gen)

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

-- Zigzag spring generation using custom polygon
-- Parameters: width, height, zigzag segments, hole radius, thickness
zigzagSpring :: D -> D -> Int -> D -> D -> Shape
zigzagSpring width height segments holeRadius thickness =
  let -- Calculate dimensions
      endWidth = 4.0 -- Width of each end square
      springLength = width - 2 * endWidth
      segmentLength = springLength / fromIntegral segments
      zigzagHeight = height * 0.6 -- Zigzag amplitude
      centerY = height / 2.0

      -- Generate zigzag points
      generateZigzag :: Int -> [(D, D)]
      generateZigzag n =
        let startX = endWidth
            points =
              map
                ( \i ->
                    let x = startX + fromIntegral i * segmentLength
                        y =
                          if even i
                            then centerY + zigzagHeight / 2.0
                            else centerY - zigzagHeight / 2.0
                     in (x, y)
                )
                [0 .. n]
         in points

      -- Create the full spring profile points
      zigzagPoints = generateZigzag segments

      -- Spring outline points (clockwise from bottom-left)
      springPoints =
        -- Left end (bottom to top)
        [(0, 0), (0, height), (endWidth, height)]
          ++
          -- Zigzag top edge
          zigzagPoints
          ++
          -- Right end (top to bottom)
          [(width, height), (width, 0), (width - endWidth, 0)]
          ++
          -- Zigzag bottom edge (reversed)
          reverse (map (\(x, y) -> (x, height - y)) zigzagPoints)
          ++
          -- Close the shape
          [(endWidth, 0)]

      -- Create the spring body polygon
      springBody = Poly (PD springPoints [[0 .. fromIntegral (length springPoints - 1)]])

      -- Create holes for the ends
      leftHole = Shape2D 100 holeRadius -- Circle profile
      rightHole = Shape2D 100 holeRadius -- Circle profile

      -- Position holes
      leftHolePos = Tx (endWidth / 2.0) (Ty (height / 2.0) leftHole)
      rightHolePos = Tx (width - endWidth / 2.0) (Ty (height / 2.0) rightHole)

      -- Extrude everything
      springBody3D = Extrude thickness springBody
      leftHole3D = Extrude thickness leftHolePos
      rightHole3D = Extrude thickness rightHolePos

      -- Subtract holes from spring body
      finalSpring = Diff (Diff springBody3D leftHole3D) rightHole3D
   in finalSpring

-- Example springs
spring1 :: Shape
spring1 = zigzagSpring 20 5 4 1.0 2.0 -- 20x5 with 4 zigzag segments

spring2 :: Shape
spring2 = zigzagSpring 25 6 6 1.2 2.5 -- 25x6 with 6 zigzag segments

spring3 :: Shape
spring3 = zigzagSpring 15 4 3 0.8 1.5 -- 15x4 with 3 zigzag segments

-- Positioned springs for display
s1 :: Shape
s1 = Ty (-8) spring1

s2 :: Shape
s2 = Ty 0 spring2

s3 :: Shape
s3 = Ty 10 spring3

-- Combined spring display
springDemo :: Shape
springDemo = Union [s1, s2, s3]

--- >>> gen gearDemo
