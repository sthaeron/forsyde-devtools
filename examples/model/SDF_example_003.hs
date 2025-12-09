module SDF_example_003 where

import ForSyDe.Shallow

-- Net list
system :: Signal Int -> Signal Int
system s_in = s_out
  where
    (s_out, s_1) = a_a s_in s_2
    s_2 = d_1 s_1

-- Process specifications
a_a :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
a_a s_1 s_2 = actor22SDF (1, 1) (1, 1) add s_1 s_2

d_1 :: Signal Int -> Signal Int
d_1 s_1 = delaySDF [0] s_1

-- Function definitions
add :: [Int] -> [Int] -> ([Int], [Int])
add [x] [y] = ([x + y], [x + y])
