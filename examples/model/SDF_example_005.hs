module SDF_example_005 where

import ForSyDe.Shallow

-- Net list
system :: Signal Int -> (Signal Int, Signal Int)
system s_in = (s_out_1, s_out_2)
  where
    (s_out_1, s_out_2) = a_a s_in

-- Process specifications
a_a :: Signal Int -> (Signal Int, Signal Int)
a_a s_1 = actor12SDF 1 (1, 1) add s_1

-- Function definitions
add :: [Int] -> ([Int], [Int])
add [x] = ([x + 1], [x + 2])
