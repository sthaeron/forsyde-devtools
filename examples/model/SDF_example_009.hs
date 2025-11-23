module SDF_example_009 where

import ForSyDe.Shallow

-- Net list
system :: Signal Int -> (Signal Int, Signal Int, Signal Int)
system s_in = (s_out_1, s_out_2, s_out_3)
  where
    (s_out_1, s_out_2, s_out_3) = a_a s_in

-- Process specifications
a_a :: Signal Int -> (Signal Int, Signal Int, Signal Int)
a_a s = actor13SDF 1 (1, 1, 1) add s

-- Function definitions
add :: [Int] -> ([Int], [Int], [Int])
add [x] = ([x + 1], [x + 2], [x + 3])
