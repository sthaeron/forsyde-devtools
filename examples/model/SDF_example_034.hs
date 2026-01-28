module SDF_example_034 where

import ForSyDe.Shallow

-- Netlist
system s_in = s_out
  where
    s_out = a_a s_in

-- Process Constructors
a_a = actor11SDF 1 1 undefined
