module SDF_example_004 where

import ForSyDe.Shallow

-- Net list
system s_in_1 s_in_2 = (s_out_2, s_out_1)
  where
    (s_out_1, s_out_2) = a_a s_in_1 s_in_2

-- Process specifications
a_a s_1 s_2 = actor22SDF (1, 1) (1, 1) add s_1 s_2

-- Function definitions
add [x] [y] = ([x + y], [x + y])
