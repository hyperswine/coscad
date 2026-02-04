module Z where

import Lib

-- | Create a zigzag spring using rectangles (simplified positioning)
zigzagSpring3D n h angleDeg lineWidth thickness =
  let
      angle = angleDeg * pi / 180.0
      dx = h / tan angle
      segmentLength = h / sin angle
      segments = [createSegment i | i <- [0 .. (2 * n - 1)]]

      createSegment i =
        let isUp = even i
            -- Starting point of each segment
            startX = fromIntegral i * dx
            startY = if isUp then 0 else h
            -- Rotation angle
            rot = if isUp then angleDeg else (-angleDeg)
            -- Create rectangle starting at origin
            rect = Rectangle segmentLength lineWidth thickness
            -- Rotate around the start point (one end of rectangle)
            rotated = Rz rot rect
            -- Translate to correct position
            positioned = Tx startX (Ty startY rotated)
         in positioned

   in Union segments

-- 5 cycles, 10mm height, 45° angle, 2mm line width, 3mm extrusion
spring = zigzagSpring3D 5 10.0 45.0 2.0 3.0


--- >>> writeScad spring "zigz/zigzag_spring_proper.scad"
