module SDF_example_020 where

import ForSyDe.Shallow

system :: Signal Int -> Signal Int
system s_in = s_out
  where
    s_1 = a_delay1 s_in
    s_out = a_add s_1

a_delay1 :: Signal Int -> Signal Int
a_delay1 s_in = delaySDF [0] s_in

a_add :: Signal Int -> Signal Int
a_add s_in = actor11SDF 1 1 f_add s_in

f_add :: [Int] -> [Int]
f_add [x] = [x + 1]

main :: IO ()
main =
  getLine >>= \line ->
    putStrLn . unwords . map show . fromSignal . system . signal . map read . words $ line
