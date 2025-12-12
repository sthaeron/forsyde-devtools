module SDF_example_010 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int -> (Signal Int, Signal Int, Signal Int, Signal Int)
system s_in_1 s_in_2 = (s_out_1, s_out_2, s_out_3, s_out_4)
  where
    (s_out_1, s_out_2, s_out_3, s_out_4) = a_a s_in_1 s_in_2

-- Process specifications
a_a :: Signal Int -> Signal Int -> (Signal Int, Signal Int, Signal Int, Signal Int)
a_a s_1 s_2 = actor24SDF (1, 1) (1, 1, 1, 1) add s_1 s_2

-- Function definitions
add :: [Int] -> [Int] -> ([Int], [Int], [Int], [Int])
add [x] [y] = ([x], [x + y], [x + y], [y])
