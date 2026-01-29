module SDF_example_018 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int
system s_in = s_out
  where
    (s_out, first, second) = a_a s_in first_delayed second_delayed
    first_delayed = d_1 first
    second_delayed = d_2 second

-- Process specifications
a_a :: Signal Int -> Signal Int -> Signal Int -> (Signal Int, Signal Int, Signal Int)
a_a s_1 s_2 s_3 = actor33SDF (1, 1, 1) (1, 1, 1) f s_1 s_2 s_3

d_1 :: Signal Int -> Signal Int
d_1 s = delaySDF [0] s

d_2 :: Signal Int -> Signal Int
d_2 s = delaySDF [1] s

-- Function definitions
f :: [Int] -> [Int] -> [Int] -> ([Int], [Int], [Int])
f [index] [first] [second] = ([index + first + second], [second], [index + first + second])
