module SDF_example_005 where

import ForSyDe.Shallow

-- -- Netlist
system s_in = (s_out1, s_out2)
  where
    (s_out1, s_out2) = a_1 s_in

-- -- Process specifications

a_1 s_1 = actor12SDF 1 (1, 1) add s_1

-- -- Function definitions
add [x] = ([x + 1], [x + 2])
