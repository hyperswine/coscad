-- Test the mathematical zigzag spring implementation
-- This shows the actual equation in action

import Examples
import Lib

main = do
  -- Generate springs using the mathematical zigzag function
  let mathSpring1 = zigzagSpring 20 5 1.0 2.0
  let mathSpring2 = zigzagSpring 25 6 1.2 2.5
  let mathSpring3 = zigzagSpring 15 4 0.8 1.5

  -- Output individual springs
  writeScad mathSpring1 "math_spring_20x5.scad"
  writeScad mathSpring2 "math_spring_25x6.scad"
  writeScad mathSpring3 "math_spring_15x4.scad"

  -- Position for comparison
  let s1 = Ty (-8) mathSpring1
  let s2 = Ty 0 mathSpring2
  let s3 = Ty 10 mathSpring3

  let springSet = Union [s1, s2, s3]
  writeScad springSet "math_spring_set.scad"

  putStrLn "Generated mathematical zigzag springs!"
  putStrLn "Using equations:"
  putStrLn "y = (dx - floor(dx)) * a + b + c/2"
  putStrLn "a = c * (-1 + 2 * mod(floor(dx), 2))"
  putStrLn "b = -c * mod(floor(dx), 2)"
  putStrLn "where c = 1.8, d = 1.0"
