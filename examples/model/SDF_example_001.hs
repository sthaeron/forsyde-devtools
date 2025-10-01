-- vi: ts=4 sts=4 sw=4 et
module SDF_example_001 where

import ForSyDe.Shallow

system :: Signal Int -> (Signal Int, Signal Int)
system s_in = (s_out1, s_out2) where
    -- Process A
    (s_a_b, s_a_c) = processA s_in
    -- Process B
    (s_out1, s_b_d) = processB s_a_b
    -- Process C
    s_c_d = processC s_a_c
    -- Process D
    s_out2 = processD s_b_d s_c_d

processA :: Signal Int -> (Signal Int, Signal Int)
processA s_in = actor12SDF 2 (1,1) f s_in where
    f [a, b] = ([2*a], [3*b])

processB :: Signal Int -> (Signal Int, Signal Int)
processB s_in = actor12SDF 1 (1, 1) f s_in where
    f [a] = ([a-2], [a-1])

processC :: Signal Int -> Signal Int
processC s_in = actor11SDF 1 1 f s_in where
    f [a] = [a+1]

processD :: Signal Int -> Signal Int -> Signal Int
processD s_in1 s_in2 = actor21SDF (1, 1) 1 f s_in1 s_in2 where
    f [a] [b] = [a*b]
