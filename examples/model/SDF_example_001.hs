module SDF_example_001 where

import ForSyDe.Shallow

system s_in = (s_out1, s_out2)
  where
    (s_1_2, s_1_3) = a_1 s_in
    (s_out1, s_2_4) = a_2 s_1_2
    s_3_4 = a_3 s_1_3
    s_out2 = a_4 s_2_4 s_3_4

a_1 s_in = actor12SDF 2 (1, 1) f_1 s_in

a_2 s_in = actor12SDF 1 (1, 1) f_2 s_in

a_3 s_in = actor11SDF 1 1 f_3 s_in

a_4 s_in1 s_in2 = actor21SDF (1, 1) 1 f_4 s_in1 s_in2

f_1 [x, y] = ([2 * x], [3 * y])

f_2 [x] = ([x - 2], [x - 1])

f_3 [x] = [x + 1]

f_4 [x] [y] = [x * y]
