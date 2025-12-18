module SDF_example_023 where

import ForSyDe.Shallow

system s_in = s_out
  where
    s_out = actor_base64 s_in

actor_base64 :: Signal Int -> Signal Int
actor_base64 s_in = actor11SDF 3 4 f_base64 s_in

-- Intentionally avoid bitwise operations
f_base64 :: [Int] -> [Int]
f_base64 [a0, a1, a2] =
  [ a0 `div` 4,
    a0 * 16 - (a0 `div` 4) * 64 + a1 `div` 16,
    a1 * 4 - (a1 `div` 16) * 64 + a2 `div` 64,
    a2 - (a2 `div` 64) * 64
  ]
