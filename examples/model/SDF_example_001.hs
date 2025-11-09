module SDF_example_001 where

import ForSyDe.Shallow

-- Net list
system :: Signal Int -> (Signal Int, Signal Int)
system s_in = (s_out_1, s_out_2)
  where
    (s_1_2, s_1_3) = a_a s_in
    (s_out_1, s_2_4) = a_b s_1_2
    s_3_4 = a_c s_1_3
    s_out_2 = a_d s_2_4 s_3_4

-- Process specifications
a_a :: Signal Int -> (Signal Int, Signal Int)
a_a s_in = actor12SDF 2 (1, 1) f_1 s_in

a_b :: Signal Int -> (Signal Int, Signal Int)
a_b s_in = actor12SDF 1 (1, 1) f_2 s_in

a_c :: Signal Int -> Signal Int
a_c s_in = actor11SDF 1 1 f_3 s_in

a_d :: Signal Int -> Signal Int -> Signal Int
a_d s_in_1 s_in_2 = actor21SDF (1, 1) 1 f_4 s_in_1 s_in_2

-- Function definitions
f_1 :: [Int] -> ([Int], [Int])
f_1 [x, y] = ([2 * x], [3 * y])

f_2 :: [Int] -> ([Int], [Int])
f_2 [x] = ([x - 2], [x - 1])

f_3 :: [Int] -> [Int]
f_3 [x] = [x + 1]

f_4 :: [Int] -> [Int] -> [Int]
f_4 [x] [y] = [x * y]
