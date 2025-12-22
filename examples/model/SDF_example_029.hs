module SDF_example_029 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int -> Signal Int
system s_in_x s_in_y = s_out
  where
    s_1 = a_a s_in_x s_in_y
    (s_out, s_2) = a_b s_1 s_2_delayed
    s_2_delayed = d_1 s_2

-- Process specifications
a_a = actor21SDF (1, 1) 1 add

a_b = actor22SDF (1, 1) (1, 1) accumulate

d_1 = delaySDF [0]

-- Function definitions
add [x] [y] = [x + y]

accumulate [x] [y] = ([x + y], [x + y])
