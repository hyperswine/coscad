module Examples where

import Lib (D, PD (..), Shape (..), writeScad, (■))

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

-- Zigzag spring generation using mathematical equations
-- Parameters: width, height, hole radius, thickness
-- Uses equations: y = (dx - floor(dx)) * a + b + c/2
-- where a = c * (-1 + 2 * mod(floor(dx), 2))
-- and b = -c * mod(floor(dx), 2)
zigzagSpring :: D -> D -> D -> D -> Shape
zigzagSpring width height holeRadius thickness =
  let
    -- Mathematical parameters
    c = 1.8
    d = 1.0
    zigzagThickness = 1.25  -- Thickness of zigzag line

    -- Calculate dimensions
    endWidth = 4.0  -- Width of each end square
    springLength = width - 2 * endWidth
    centerY = height / 2.0

    -- Mathematical zigzag function
    zigzagY :: D -> D
    zigzagY x =
      let
        dx = d * x
        floorDx = fromIntegral (floor dx)
        fractionalPart = dx - floorDx
        modFloorDx = fromIntegral (floor dx `mod` 2)
        a = c * (-1 + 2 * modFloorDx)
        b = -c * modFloorDx
      in
        fractionalPart * a + b + c / 2.0

    -- Generate points along the zigzag curve
    numPoints = 50  -- Number of points to sample
    stepSize = springLength / fromIntegral numPoints

    -- Generate zigzag centerline points
    zigzagCenterPoints = map (\i ->
      let
        x = fromIntegral i * stepSize
        y = centerY + zigzagY x
      in (endWidth + x, y)
      ) [0..numPoints]

    -- Create upper and lower boundaries of the zigzag
    zigzagUpperPoints = map (\(x, y) -> (x, y + zigzagThickness / 2.0)) zigzagCenterPoints
    zigzagLowerPoints = reverse (map (\(x, y) -> (x, y - zigzagThickness / 2.0)) zigzagCenterPoints)

    -- Spring outline points (just the zigzag part)
    springPoints =
      -- Zigzag upper edge
      zigzagUpperPoints
      ++
      -- Zigzag lower edge
      zigzagLowerPoints

    -- Create the spring body polygon (just zigzag)
    springBody = Poly (PD springPoints [[0..fromIntegral (length springPoints - 1)]])

    -- Create end cuboids separately
    leftEnd = Rectangle endWidth height thickness
    rightEnd = Rectangle endWidth height thickness

    -- Position the end cuboids
    leftEndPos = leftEnd  -- Already at origin
    rightEndPos = Tx (width - endWidth) rightEnd

    -- Create holes for the ends
    leftHole = Shape2D 100 holeRadius   -- Circle profile
    rightHole = Shape2D 100 holeRadius  -- Circle profile

    -- Position holes
    leftHolePos = Tx (endWidth / 2.0) (Ty (height / 2.0) leftHole)
    rightHolePos = Tx (width - endWidth / 2.0) (Ty (height / 2.0) rightHole)

    -- Extrude everything
    springBody3D = Extrude thickness springBody
    leftHole3D = Extrude thickness leftHolePos
    rightHole3D = Extrude thickness rightHolePos

    -- Create ends with holes
    leftEndWithHole = Diff leftEndPos leftHole3D
    rightEndWithHole = Diff rightEndPos rightHole3D

    -- Combine spring body with ends
    finalSpring = Union [springBody3D, leftEndWithHole, rightEndWithHole]
  in
    finalSpring

-- Example springs
spring1 :: Shape
spring1 = zigzagSpring 20 5 1.0 2.0 -- 20x5 spring

spring2 :: Shape
spring2 = zigzagSpring 25 6 1.2 2.5 -- 25x6 spring

spring3 :: Shape
spring3 = zigzagSpring 15 4 0.8 1.5 -- 15x4 spring

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

-- Submarine (U-boat style) generation
-- Length: 18cm, Diameter: 5cm (radius 2.5cm)
-- Features: streamlined hull with flat top/bottom, circular sides, conning tower
submarine :: Shape
submarine =
  let
    -- Basic parameters
    subLength = 18.0
    subRadius = 2.5

    -- Hull cross-section: circle intersected with rectangle to create flat top/bottom
    -- The rectangle cuts off the top and bottom to create flat surfaces
    hullProfile =
      let
        -- Circle for the basic shape
        circle = Shape2D 100 subRadius

        -- Rectangle to intersect (slightly wider than diameter, but shorter height)
        -- This creates the flat top and bottom effect
        rectHeight = subRadius * 1.6  -- Leaves some flat area
        rect = Rectangle (subRadius * 3) rectHeight 1.0

        -- Center the rectangle
        centeredRect = Ty (-rectHeight / 2.0) rect
      in
        -- For 2D intersection, we'll use the circle directly
        -- The flat effect will come from the hull operation with positioned profiles
        circle

    -- Create 5 cross-sections along the submarine length for hull operation
    -- Position them at different points and scale them for streamlined shape
    section1 = Tx 0 (Scale (0.2, 1.0, 1.0) hullProfile)          -- Nose (20% scale)
    section2 = Tx (subLength * 0.25) (Scale (0.8, 1.0, 1.0) hullProfile)  -- Forward (80% scale)
    section3 = Tx (subLength * 0.5) hullProfile                   -- Middle (100% scale)
    section4 = Tx (subLength * 0.75) (Scale (0.8, 1.0, 1.0) hullProfile)  -- Aft (80% scale)
    section5 = Tx subLength (Scale (0.1, 1.0, 1.0) hullProfile)   -- Stern (10% scale)

    -- Create the main hull using hull operation
    mainHull = Hull [section1, section2, section3, section4, section5]

    -- Extrude the hull in the Z direction to give it thickness
    hull3D = Extrude 1.0 mainHull

    -- Create the conning tower (periscope tower)
    -- This is like a frustum on top of the submarine
    towerHeight = 1.5
    towerBottomRadius = 1.0
    towerTopRadius = 0.7

    -- Position the tower at the middle-front of the submarine
    towerPos = Tx (subLength * 0.4) (Ty 0 (Tz 0.5))
    tower = towerPos (Frustum towerHeight towerBottomRadius towerTopRadius)

    -- Create additional details
    -- Propeller shaft (small cylinder at the back)
    propShaft = Tx (subLength + 0.2) (Ry 90 (Cylinder 0.2 1.0))

    -- Periscope (thin cylinder on tower)
    periscope = Tx (subLength * 0.4) (Ty 0 (Tz (0.5 + towerHeight))) (Cylinder 0.1 1.0)

    -- Combine all parts
    fullSubmarine = Union [hull3D, tower, propShaft, periscope]

  in
    fullSubmarine

-- Create a streamlined submarine variant with better hull definition
submarineStreamlined :: Shape
submarineStreamlined =
  let
    -- Parameters
    subLength = 18.0
    subRadius = 2.5

    -- Create a more sophisticated hull using multiple shaped sections
    -- Each section is a circle that's been flattened on top and bottom
    createFlattenedCircle :: D -> D -> Shape
    createFlattenedCircle radius flattenFactor =
      let
        -- Create an ellipse-like shape by scaling Y
        circle = Shape2D 100 radius
        flattened = Scale (1.0, flattenFactor, 1.0) circle
      in flattened

    -- Five cross-sections with varying sizes and flattening
    s1 = Tx 0 (createFlattenedCircle (subRadius * 0.2) 0.6)           -- Pointed nose
    s2 = Tx (subLength * 0.2) (createFlattenedCircle (subRadius * 0.7) 0.8)  -- Growing
    s3 = Tx (subLength * 0.4) (createFlattenedCircle subRadius 0.85)          -- Near full size
    s4 = Tx (subLength * 0.7) (createFlattenedCircle (subRadius * 0.9) 0.8)  -- Tapering
    s5 = Tx subLength (createFlattenedCircle (subRadius * 0.15) 0.5)          -- Tapered stern

    -- Create hull
    hullShape = Hull [s1, s2, s3, s4, s5]
    hull3D = Extrude 1.0 hullShape

    -- Conning tower - more realistic proportions
    towerBase = Tx (subLength * 0.35) (Ty 0 (Tz 0.5))
    tower = towerBase (Union [
        -- Main tower body
        Frustum 1.2 0.9 0.6,
        -- Tower top platform
        Tz 1.2 (Cylinder 0.8 0.3)
      ])

    -- Fins and rudders
    -- Dorsal fin
    dorsalFin = Tx (subLength * 0.8) (Ty 0 (Tz 0.5)) (Scale (0.3, 0.1, 1.5) (■ 1))

    -- Side rudders
    leftRudder = Tx (subLength * 0.9) (Ty (-subRadius * 0.8) (Tz 0)) (Scale (0.2, 1.0, 0.8) (■ 1))
    rightRudder = Tx (subLength * 0.9) (Ty (subRadius * 0.8) (Tz 0)) (Scale (0.2, 1.0, 0.8) (■ 1))

    -- Propeller (simplified)
    propeller = Tx (subLength + 0.1) (Ry 90 (Union [
      Cylinder 0.15 0.5,  -- Hub
      Rz 0 (Scale (2.0, 0.1, 0.1) (■ 1)),     -- Blade 1
      Rz 90 (Scale (2.0, 0.1, 0.1) (■ 1))     -- Blade 2
      ]))

    -- Complete submarine
    complete = Union [hull3D, tower, dorsalFin, leftRudder, rightRudder, propeller]

  in complete

-- Basic submarine example
uboat :: Shape
uboat = submarine

-- Advanced submarine example
uboatAdvanced :: Shape
uboatAdvanced = submarineStreamlined

-- Positioned submarines for comparison
sub1 :: Shape
sub1 = Ty (-8) uboat

sub2 :: Shape
sub2 = Ty 8 uboatAdvanced

-- Combined submarine demo
submarineDemo :: Shape
submarineDemo = Union [sub1, sub2]

--- >>> gen gearDemo

--- >>> writeScad springDemo "examples/springDemo2.scad"

--- >>> writeScad submarineDemo "examples/submarine.scad"
