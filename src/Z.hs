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
      points = [(i * dx, if odd i then h else 0) | i <- [0 .. (2 * n)]]

      -- Connect all points in sequence
      paths = [[0 .. (2 * n)]]

  in Poly (PD points paths)

-- | Zigzag spring with thickness (extruded version)
zigzagSpring3D :: Int -> D -> D -> D -> Shape
zigzagSpring3D n h angleDeg thickness =
  Extrude thickness (zigzagSpring n h angleDeg)

-- | Zigzag spring with rounded thickness using offset
zigzagSpringRounded :: Int -> D -> D -> D -> Shape
zigzagSpringRounded n h angleDeg thickness =
  Extrude thickness (Offset (thickness / 2) (zigzagSpring n h angleDeg))
