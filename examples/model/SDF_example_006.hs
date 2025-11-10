module SDF_example_006 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int
system s_in = s_out
  where
    (s_1, s_2, s_3, s_4) = a_a s_in
    s_5 = a_b s_1 s_2
    (s_6, s_7) = a_c s_3
    s_8 = a_d s_5 s_6
    s_out = a_e s_8 s_7 s_4

-- Process specifications
a_a :: Signal Int -> (Signal Int, Signal Int, Signal Int, Signal Int)
a_a s_1 = actor14SDF 2 (1, 3, 2, 4) f_1 s_1

a_b :: Signal Int -> Signal Int -> Signal Int
a_b s_1 s_2 = actor21SDF (1, 3) 2 f_2 s_1 s_2

a_c :: Signal Int -> (Signal Int, Signal Int)
a_c s_1 = actor12SDF 2 (2, 1) f_3 s_1

a_d :: Signal Int -> Signal Int -> Signal Int
a_d s_1 s_2 = actor21SDF (2, 2) 2 f_4 s_1 s_2

a_e :: Signal Int -> Signal Int -> Signal Int -> Signal Int
a_e s_1 s_2 s_3 = actor31SDF (2, 1, 4) 1 f_5 s_1 s_2 s_3

-- Function definitions
f_1 :: [Int] -> ([Int], [Int], [Int], [Int])
f_1 [x, y] = ([x], [y, x, y + 2], [x + y, x + 1], [x + y + 2, x + 3, y, y + 1])

f_2 :: [Int] -> [Int] -> [Int]
f_2 [x] [y, z, w] = [x + y + 2, z + w + 1]

f_3 :: [Int] -> ([Int], [Int])
f_3 [x, y] = ([x + y + 1, x], [x + y + 2])

f_4 :: [Int] -> [Int] -> [Int]
f_4 [x, y] [z, w] = [x + z, y + w]

f_5 :: [Int] -> [Int] -> [Int] -> [Int]
f_5 [x, y] [z] [w, v, q, r] = [x + y + z + w + v + q + r]
