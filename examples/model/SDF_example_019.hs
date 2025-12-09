module SDF_example_019 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int -> Signal Int
system s_in_1 s_in_2 = s_out
  where
    s_1 = a_a s_in_1 s_in_2
    s_out = a_b s_1

-- Process specifications
a_a :: Signal Int -> Signal Int -> Signal Int
a_a s_1 s_2 = actor21SDF (1, 1) 1 multiply s_1 s_2

a_b :: Signal Int -> Signal Int
a_b s = actor11SDF 1 1 negating s

-- Function definitions
multiply :: [Int] -> [Int] -> [Int]
multiply [x] [y] = [(x + y) * 4]

negating :: [Int] -> [Int]
negating [x] = [-x]
