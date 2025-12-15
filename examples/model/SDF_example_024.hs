module SDF_example_024 where

import ForSyDe.Shallow

system :: Signal Int -> (Signal Int, Signal Int, Signal Int)
system s_in = (s_full, s_partial3, s_partial2)
  where
    (s_in1, s_in2, s_in3) = actor_3split s_in
    s_pre_partial3_delayed = delay_partial_3 s_in2
    s_partial3 = actor_base64_partial_3 s_pre_partial3_delayed
    s_pre_partial2_delayed = delay_partial_2 s_in3
    s_partial2 = actor_base64_partial_2 s_pre_partial2_delayed
    s_full = actor_base64 s_in1

actor_base64 :: Signal Int -> Signal Int
actor_base64 s_in = actor11SDF 3 4 f_base64 s_in

actor_base64_partial_3 :: Signal Int -> Signal Int
actor_base64_partial_3 s_in = actor11SDF 3 4 f_base64_partial_3 s_in

actor_base64_partial_2 :: Signal Int -> Signal Int
actor_base64_partial_2 s_in = actor11SDF 3 4 f_base64_partial_2 s_in

actor_3split :: Signal Int -> (Signal Int, Signal Int, Signal Int)
actor_3split s_in = actor13SDF 1 (1, 1, 1) f_3split s_in

delay_partial_3 :: Signal Int -> Signal Int
delay_partial_3 s_in = delaySDF [0] s_in

delay_partial_2 :: Signal Int -> Signal Int
delay_partial_2 s_in = delaySDF [0, 0] s_in

f_3split :: [Int] -> ([Int], [Int], [Int])
f_3split [x] = ([x], [x], [x])

-- Intentionally avoid bitwise operations
f_base64 :: [Int] -> [Int]
f_base64 [a0, a1, a2] =
  [ a0 `div` 4,
    a0 * 16 - (a0 `div` 4) * 64 + a1 `div` 16,
    a1 * 4 - (a1 `div` 16) * 64 + a2 `div` 64,
    a2 - (a2 `div` 64) * 64
  ]

f_base64_partial_3 :: [Int] -> [Int]
f_base64_partial_3 [_, a0, a1] =
  [ a0 `div` 4,
    a0 * 16 - (a0 `div` 4) * 64 + a1 `div` 16,
    a1 * 4 - (a1 `div` 16) * 64,
    -1
  ]

f_base64_partial_2 :: [Int] -> [Int]
f_base64_partial_2 [_, _, a0] =
  [ a0 `div` 4,
    a0 * 16 - (a0 `div` 4) * 64,
    -1,
    -1
  ]
