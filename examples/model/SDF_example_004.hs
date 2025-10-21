module SDF_example_004 where

import ForSyDe.Shallow

-- -- Netlist
system s_in1 s_in2 = (s_out2, s_out1)
  where
    (s_out1, s_out2) = a_1 s_in1 s_in2

-- -- Process specifications
a_1 s_1 s_2 = actor22SDF (1, 1) (1, 1) add s_1 s_2

add [x] [y] = ([x + y], [x + y])
