module SDF_example_016 where

import ForSyDe.Shallow

-- Net list
system :: Signal Int -> Signal Int -> Signal Int
system s_in_1 s_in_2 = s_out
  where
    s_out = a_a s_in_1 s_in_2

-- Process specifications
a_a :: Signal Int -> Signal Int -> Signal Int
a_a s_1 s_2 = actor21SDF (2, 2) 1 add s_1 s_2

-- Function definitions
add :: [Int] -> [Int] -> [Int]
add [x, y] [z, w] = [x + z, y + w]
