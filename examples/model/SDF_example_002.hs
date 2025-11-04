module SDF_example_002 where

import ForSyDe.Shallow

-- Netlist
system s_ina s_inb = s_out
  where
    s_1 = a_1 s_ina
    s_2 = a_2 s_inb
    s_3 = a_3 s_1 s_4_delayed
    (s_out, s_4) = a_4 s_2 s_3
    s_4_delayed = d_1 s_4

-- Process specifications
a_1 s1 = actor11SDF 2 1 f_1 s1

a_2 s1 = actor11SDF 1 2 f_2 s1

a_3 s1 s2 = actor21SDF (2, 1) 1 f_3 s1 s2

a_4 s1 s2 = actor22SDF (2, 1) (1, 2) f_4 s1 s2

d_1 s = delaySDF [0] s

-- Function definitions
f_1 [x, y] = [x + y]

f_2 [x] = [x, x + 1]

f_3 [x, y] [z] = [x + y + z]

f_4 [x, y] [z] = ([x + y + z], [x + y, x + y + z])
