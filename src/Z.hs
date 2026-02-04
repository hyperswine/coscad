module Z where

import Lib

-- | Create a zigzag spring with proper thickness (manual offsetting)
zigzagSpring :: Int -> D -> D -> D -> Shape
zigzagSpring n h angleDeg lineWidth =
  let angle = angleDeg * pi / 180.0
      dx = h / tan angle
      halfWidth = lineWidth / 2

      -- Calculate perpendicular offset direction for each segment
      -- For a line at angle α, perpendicular is at (α + 90°)
      perpX = sin angle * halfWidth
      perpY = cos angle * halfWidth

      -- Generate centerline points
      centerPoints = [(fromIntegral i * dx, if odd i then h else 0) | i <- [0 .. (2 * n)]]

      -- Offset points outward (top edge)
      topPoints = [if odd i
                   then (x - perpX, y + perpY)  -- going up-right, offset left
                   else (x + perpX, y + perpY)  -- going down-right, offset right
                   | (i, (x, y)) <- zip [0..] centerPoints]

      -- Offset points inward (bottom edge)
      bottomPoints = reverse [if odd i
                              then (x + perpX, y - perpY)
                              else (x - perpX, y - perpY)
                              | (i, (x, y)) <- zip [0..] centerPoints]

      -- Combine to create closed shape
      allPoints = topPoints ++ bottomPoints
      paths = [[0 .. (length allPoints - 1)]] |> map (map fromIntegral)

  in Poly (PD allPoints paths)

-- | 3D version
zigzagSpring3D :: Int -> D -> D -> D -> D -> Shape
zigzagSpring3D n h angleDeg lineWidth thickness =
  Extrude thickness (zigzagSpring n h angleDeg lineWidth)

-- 5 cycles, 10mm height, 45° angle, 2mm line width, 3mm extrusion
spring = zigzagSpring3D 5 10.0 45.0 2.0 3.0

--- >>> writeScad spring "zigz/zigzag_spring_proper.scad"
