module SDF_example_002 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int -> Signal Int
system s_ina s_inb = s_out where
    s_1 = actor_a s_ina
    s_2 = actor_b s_inb
    s_3 = actor_c s_1 s_4_delayed
    (s_4, s_out) = actor_d s_2 s_3
    s_4_delayed = d_1 s_4

-- Process specifications
actor_a :: Signal Int -> Signal Int
actor_a s1 = actor11SDF 2 1 f_1 s1
actor_b :: Signal Int -> Signal Int
actor_b s1 = actor11SDF 1 2 f_2 s1
actor_c :: Signal Int -> Signal Int -> Signal Int
actor_c s1 s2 = actor21SDF (2,1) 1 f_3 s1 s2
actor_d :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
actor_d s1 s2 = actor22SDF (2,1) (1,2) f_4 s1 s2
d_1 :: Signal Int -> Signal Int
d_1 s = delaySDF [0] s 

-- Function definitions
f_1 :: [Int] -> [Int]
f_1 [x, y] = [x + y]
f_2 :: [Int] -> [Int]
f_2 [x] = [x, x + 1]
f_3 :: [Int] -> [Int] -> [Int]
f_3 [x, y] [z] = [x + y + z]
f_4 :: [Int] -> [Int] -> ([Int], [Int])
f_4 [x, y] [z] = ([x + y + z], [x + y, x + y + z])