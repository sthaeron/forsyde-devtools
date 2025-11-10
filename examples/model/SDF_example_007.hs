module SDF_example_007 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int -> Signal Int
system s_in_a s_in_b = s_out
  where
    (s_1, s_2) = a_a s_in_a
    (s_3, s_4, s_5) = a_b s_in_b
    (s_6, s_7) = a_c s_2 s_3 s_4
    s_out = a_d s_1 s_6 s_7 s_5

-- Process specifications
a_a :: Signal Int -> (Signal Int, Signal Int)
a_a s_1 = actor12SDF 2 (2, 1) f_1 s_1

a_b :: Signal Int -> (Signal Int, Signal Int, Signal Int)
a_b s_1 = actor13SDF 1 (2, 1, 2) f_2 s_1

a_c :: Signal Int -> Signal Int -> Signal Int -> (Signal Int, Signal Int)
a_c s_1 s_2 s_3 = actor32SDF (2, 4, 2) (2, 2) f_3 s_1 s_2 s_3

a_d :: Signal Int -> Signal Int -> Signal Int -> Signal Int -> Signal Int
a_d s_1 s_2 s_3 s_4 = actor41SDF (4, 2, 2, 4) 2 f_4 s_1 s_2 s_3 s_4

-- Function definitions
f_1 :: [Int] -> ([Int], [Int])
f_1 [x, y] = ([x + y, y], [x])

f_2 :: [Int] -> ([Int], [Int], [Int])
f_2 [x] = ([x, x + 1], [x], [x + 2, x + 1])

f_3 :: [Int] -> [Int] -> [Int] -> ([Int], [Int])
f_3 [x, y] [z, v, w, q] [r, t] = ([x + y + v, y], [z + w + q + r + t, z])

f_4 :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
f_4 [x, y, z, v] [w, q] [r, t] [u, m, n, o] = [x + y + z + m + n + o + 5, v + w + q + r + t + u + 1]
