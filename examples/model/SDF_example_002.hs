module SDF_example_002 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int -> Signal Int
system s_in_1 s_in_2 = s_out
  where
    s_1 = a_a s_in_1
    s_2 = a_b s_in_2
    s_3 = a_c s_1 s_4_delayed
    (s_out, s_4) = a_d s_2 s_3
    s_4_delayed = d_1 s_4

-- Process specifications
a_a :: Signal Int -> Signal Int
a_a s_1 = actor11SDF 2 1 f_1 s_1

a_b :: Signal Int -> Signal Int
a_b s_1 = actor11SDF 1 2 f_2 s_1

a_c :: Signal Int -> Signal Int -> Signal Int
a_c s_1 s_2 = actor21SDF (2, 1) 1 f_3 s_1 s_2

a_d :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
a_d s_1 s_2 = actor22SDF (2, 1) (2, 1) f_4 s_1 s_2

d_1 :: Signal Int -> Signal Int
d_1 s = delaySDF [0] s

-- Function definitions
f_1 :: [Int] -> [Int]
f_1 [x, y] = [x + y]

f_2 :: [Int] -> [Int]
f_2 [x] = [x, x + 1]

f_3 :: [Int] -> [Int] -> [Int]
f_3 [x, y] [z] = [x + y + z]

f_4 :: [Int] -> [Int] -> ([Int], [Int])
f_4 [x, y] [z] = ([x + y + z], [x + y, x + y + z])
