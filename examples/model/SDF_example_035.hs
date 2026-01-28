module SDF_example_035 where

import ForSyDe.Shallow

-- Netlist
system s_in_x s_in_y = (s_out_x, s_out_y)
  where
    (s_out_x, s_out_y) = a_a s_in_x s_in_y

-- Process Constructors
-- a_a :: Num a => Signal a -> Signal a -> (Signal a, Signal a)
a_a = actor22SDF (1, 1) (1, 1) undefined
