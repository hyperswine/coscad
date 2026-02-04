module Z where

import Lib

-- | Create a zigzag spring profile (2D)
-- n: number of complete zigzag cycles
-- h: height of each zig
-- angle: angle from horizontal in degrees (typically 30-60)
zigzagSpring :: Int -> D -> D -> Shape
zigzagSpring n h angleDeg =
  let angle = angleDeg * pi / 180.0
      dx = h / tan angle  -- horizontal distance per zig

      -- Generate points for the zigzag path
      points = [(fromIntegral i * dx, if odd i then h else 0) | i <- [0 .. (2 * n)]]

      -- Connect all points in sequence
      paths = [[0 .. (2 * n)]] |> map (map fromIntegral)

  in Poly (PD points paths)

-- | Zigzag spring with thickness (extruded version)
zigzagSpring3D :: Int -> D -> D -> D -> Shape
zigzagSpring3D n h angleDeg thickness =
  Extrude thickness (zigzagSpring n h angleDeg)

-- | Zigzag spring with rounded thickness using offset
zigzagSpringRounded :: Int -> D -> D -> D -> Shape
zigzagSpringRounded n h angleDeg thickness =
  Extrude thickness (Offset (thickness / 2) (zigzagSpring n h angleDeg))

-- Basic zigzag spring: 5 cycles, 10mm height, 45° angle
spring1 = zigzagSpring 5 10.0 45.0

-- 3D version with 2mm thickness
spring2 = zigzagSpring3D 5 10.0 45.0 2.0

-- Rounded version (smoother corners)
spring3 = zigzagSpringRounded 5 10.0 45.0 2.0

-- Steeper angle (60°) makes tighter zigzag
spring4 = zigzagSpring3D 8 8.0 60.0 1.5

-- Shallower angle (30°) makes more stretched zigzag
spring5 = zigzagSpring3D 8 8.0 30.0 1.5

-- Write to file
--- >>> writeScad spring3 "examples/zigz/zigzag_spring.scad"
