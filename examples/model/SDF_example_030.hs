module SDF_example_030 where

import ForSyDe.Shallow

-- Netlist
system s_in_x s_in_y = s_out
  where
    s_1 = a_a s_in_x s_in_y
    (s_out, s_2) = a_b s_1 s_2_delayed
    s_2_delayed = d_1 s_2

-- Process specifications
a_a = actor21SDF (1, 1) 1 add
  where
    add [x] [y] = [x + y]

a_b :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
a_b s_1 s_2 = actor22SDF (1, 1) (1, 1) accumulate s_1 s_2
  where
    accumulate [x] [y] = ([x + y], [x + y])

d_1 s = delaySDF [0] s
