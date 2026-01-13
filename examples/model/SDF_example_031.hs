module SDF_example_031 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int -> Signal Int
system s_in_x s_in_y = s_out
  where
    s_1 = a_a s_in_x s_in_y
    (s_out, s_2) = a_b s_1 s_2_delayed
    s_2_delayed = d_1 s_2

-- Process specifications
a_a :: Signal Int -> Signal Int -> Signal Int
a_a s_1 s_2 = actor21SDF (1, 1) 1 add s_1 s_2

a_b :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
a_b s_1 s_2 = actor22SDF (1, 1) (1, 1) accumulate s_1 s_2

d_1 :: Signal Int -> Signal Int
d_1 s = delaySDF [0] s

-- Function definitions
add :: [Int] -> [Int] -> [Int]
add x y = undefined

accumulate :: [Int] -> [Int] -> ([Int], [Int])
accumulate x y = undefined
