module SDF_example_026 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int
system s_in = s_out
  where
    (s_1, s_2) = a_1 s_in s_5_delayed
    s_3 = a_2 s_1
    s_4 = a_3 s_2
    (s_out, s_5) = a_4 s_3 s_4
    s_5_delayed = d_1 s_5

-- Process specifications
a_1 :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
a_1 s_1 s_2 = actor22SDF (2, 1) (2, 1) f_1 s_1 s_2

a_2 :: Signal Int -> Signal Int
a_2 s_1 = actor11SDF 2 2 f_2 s_1

a_3 :: Signal Int -> Signal Int
a_3 s_1 = actor11SDF 1 1 f_3 s_1

a_4 :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
a_4 s_1 s_2 = actor22SDF (2, 1) (1, 1) f_4 s_1 s_2

d_1 :: Signal Int -> Signal Int
d_1 s_1 = delaySDF [0] s_1

-- Function definitions
f_1 :: [Int] -> [Int] -> ([Int], [Int])
f_1 [x1, x2] [y] = ([x1, x2], [y])

f_2 :: [Int] -> [Int]
f_2 [x, y] = [x + 1, y + 1]

f_3 :: [Int] -> [Int]
f_3 [x] = [x + 1]

f_4 :: [Int] -> [Int] -> ([Int], [Int])
f_4 [x1, x2] [y] = ([(x1 + x2) * 2 - (x1 + x2) + y], [y])
