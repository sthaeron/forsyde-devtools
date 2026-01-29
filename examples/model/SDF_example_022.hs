module SDF_example_022 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int
system = s_out
  where
    (first, second) = a_a first_delayed1 second_delayed
    first_delayed = d_1 first
    (first_delayed1, s_out) = a_split first_delayed
    second_delayed = d_2 second

-- Process specifications
a_a :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
a_a s_1 s_2 = actor22SDF (1, 1) (1, 1) f s_1 s_2

a_split :: Signal Int -> (Signal Int, Signal Int)
a_split s_1 = actor12SDF 1 (1, 1) f_split s_1

d_1 :: Signal Int -> Signal Int
d_1 s = delaySDF [0] s

d_2 :: Signal Int -> Signal Int
d_2 s = delaySDF [1] s

-- Function definitions
f :: [Int] -> [Int] -> ([Int], [Int])
f [first] [second] = ([second], [first + second])

f_split :: [Int] -> ([Int], [Int])
f_split [x] = ([x], [x])
