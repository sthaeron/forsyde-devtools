module SDF_example_012 where

import ForSyDe.Shallow

-- Net list
system :: Signal Int -> Signal Int
system s_in = s_out
  where
    s_out = a_a s_in

-- Process specifications
a_a :: Signal Int -> Signal Int
a_a s = actor11SDF 2 1 add s

-- Function definitions
add :: [Int] -> [Int]
add [x, y] = [x + y]
