module SDF_example_033 where

import ForSyDe.Shallow

system s_in_x s_in_y = s_out
  where
    s_1 = a_a s_in_x s_in_y
    (s_out, s_2) = a_b s_1 s_2_delayed
    s_2_delayed = d_1 s_2

-- Process specifications
-- a_a :: Signal Int -> Signal Int -> Signal Int
a_a = actor21SDF (1, 1) 1 undefined

-- a_b :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
a_b = actor22SDF (1, 1) (1, 1) undefined

-- d_1 :: Signal Int -> Signal Int
d_1 = delaySDF [0]
