module Examples2 where

import Lib (PD (..), Shape (..), writeScad)

-- Bookend design
-- Total dimensions: 15cm tall, 8cm wide
-- Features: vertical section, protruding base slab with chamfered tip, rear support
bookend =
  let -- Overall dimensions
      totalWidth = 8.0 -- 8cm wide

      -- Main vertical section dimensions
      verticalHeight = 12.0 -- Height of the main vertical part
      verticalWidth = 1.5 -- Thickness of the vertical wall

      -- Base slab dimensions
      baseHeight = 1.0 -- Height of the base slab
      baseLength = 6.0 -- Length of protruding slab
      chamferLength = 2.0 -- Length of the chamfered tip

      -- Rear support module dimensions
      supportWidth = 3.0 -- Width of rear support
      supportHeight = 8.0 -- Height of rear support (shorter than vertical section)
      supportDepth = 2.5 -- Depth of rear support

      -- Create the main vertical section (back wall that books lean against)
      verticalSection = Rectangle verticalWidth totalWidth verticalHeight

      -- Create the base slab with chamfered tip using a custom polygon
      -- The slab profile from the side view looks like a rectangle with a triangular chamfer
      slabProfile =
        let -- Points for the slab profile (viewed from the side)
            -- Starting from bottom-left, going counter-clockwise
            points =
              [ (0.0, 0.0), -- Bottom-left corner
                (baseLength - chamferLength, 0.0), -- Start of chamfer
                (baseLength, baseHeight * 0.5), -- Tip of chamfer (shallow slope)
                (baseLength - chamferLength, baseHeight), -- Top of chamfer start
                (0.0, baseHeight) -- Top-left corner
              ]
            -- Path connecting all points
            paths = [[0, 1, 2, 3, 4]]
         in Poly (PD points paths)

      -- Extrude the slab profile to create the 3D base slab
      baseSlab = Extrude totalWidth slabProfile

      -- Create the rear support module (provides structural support)
      rearSupport = Rectangle supportDepth supportWidth supportHeight

      -- Position the rear support at the back corner
      positionedRearSupport = Tx (verticalWidth - supportDepth) rearSupport

      -- Position the base slab to protrude forward from the vertical section
      positionedBaseSlab = Tx verticalWidth baseSlab

      -- Position the vertical section and raise it above the base
      positionedVertical = Tz baseHeight verticalSection

      -- Combine all parts
      completeBookend = Union [positionedVertical, positionedBaseSlab, positionedRearSupport]
   in completeBookend

-- Alternative bookend with more detailed chamfer using hull operation
bookendDetailed =
  let -- Dimensions
      totalWidth = 8.0

      verticalHeight = 12.0
      verticalWidth = 1.5

      baseHeight = 1.0
      baseLength = 6.0

      supportWidth = 3.0
      supportHeight = 8.0
      supportDepth = baseLength + verticalWidth  -- Extend to cover full length

      -- Create vertical section
      verticalSection = Rectangle verticalWidth totalWidth verticalHeight
      positionedVertical = Tz baseHeight verticalSection

      -- Create base slab with right-angled triangle tip
      -- Main flat base rectangle
      flatBase = Rectangle (baseLength - 1.5) totalWidth baseHeight

      -- Right-angled triangle tip profile
      triangleTipProfile =
        let
          -- Right triangle: long base (1.5), short height (baseHeight), hypotenuse connects them
          points = [
            (0.0, 0.0),           -- Bottom-left (start of triangle)
            (1.5, 0.0),           -- Bottom-right (end of base - tip point)
            (0.0, baseHeight)     -- Top-left (forms right angle)
            ]
          paths = [[0, 1, 2]]
        in Poly (PD points paths)

      -- Extrude the triangle tip
      triangleTip = Extrude totalWidth triangleTipProfile

      -- Position the triangle tip at the end of the flat base
      positionedTriangleTip = Tx (baseLength - 1.5) triangleTip

      -- Combine flat base and triangle tip to form complete slab
      baseSlab = Union [flatBase, positionedTriangleTip]
      positionedBaseSlab = Tx verticalWidth baseSlab

      -- Rear support - extends full length to eliminate gap
      rearSupport = Rectangle supportDepth supportWidth supportHeight
      -- Position at the very back, covering from vertical wall to end of slab
      positionedRearSupport = Tx 0 rearSupport

      -- Combine all parts
      completeBookend = Union [positionedVertical, positionedBaseSlab, positionedRearSupport]
   in completeBookend

-- Bookend with rounded edges for aesthetic appeal
bookendRounded =
  let -- Dimensions
      totalWidth = 8.0

      verticalHeight = 12.0
      verticalWidth = 1.8 -- Slightly thicker for rounded version
      baseHeight = 1.2

      supportWidth = 3.5
      supportHeight = 8.5
      supportDepth = 2.8

      -- Main components
      verticalSection = Rectangle verticalWidth totalWidth verticalHeight

      -- Create chamfered base using a more complex polygon
      chamferProfile =
        let points =
              [ (0.0, 0.0),
                (4.5, 0.0),
                (6.0, 0.3), -- Gentler slope
                (6.5, 0.6), -- Tip point
                (6.0, 1.2), -- Top of chamfer
                (4.5, 1.2),
                (0.0, 1.2)
              ]
            paths = [[0, 1, 2, 3, 4, 5, 6]]
         in Poly (PD points paths)

      baseSlab = Extrude totalWidth chamferProfile

      -- Rear support with slight taper
      rearSupportBottom = Rectangle supportDepth supportWidth 1.0
      rearSupportTop = Rectangle (supportDepth * 0.8) (supportWidth * 0.9) 1.0
      positionedRearSupportTop = Tx (supportDepth * 0.1) (Ty (supportWidth * 0.05) (Tz (supportHeight - 1.0) rearSupportTop))

      rearSupport = Hull [rearSupportBottom, positionedRearSupportTop]

      -- Position all components
      positionedVertical = Tz baseHeight verticalSection
      positionedBaseSlab = Tx verticalWidth baseSlab
      positionedRearSupport = Tx (verticalWidth - supportDepth) rearSupport

      completeBookend = Union [positionedVertical, positionedBaseSlab, positionedRearSupport]
   in completeBookend

-- Example bookends for testing
basicBookend = bookend

detailedBookend = bookendDetailed

roundedBookend = bookendRounded

-- Positioned bookends for comparison
bookend1 = Ty (-12) basicBookend

bookend2 = Ty 0 detailedBookend

bookend3 = Ty 12 roundedBookend

-- Combined bookend demo
bookendDemo = Union [bookend1, bookend2, bookend3]

--- >>> writeScad bookendDemo "examples/bookend_demo.scad"

--- >>> writeScad basicBookend "examples/bookend_basic.scad"

--- >>> writeScad detailedBookend "examples/bookend_detailed.scad"

--- >>> writeScad roundedBookend "examples/bookend_rounded.scad"
