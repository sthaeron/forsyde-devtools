module SDF_example_011 where

import ForSyDe.Shallow

-- Netlist
system :: Signal Int -> Signal Int
system s_in = s_out
  where
    s_out = a_a s_in

-- Process specifications
a_a :: Signal Int -> Signal Int
a_a s = actor11SDF 1 1 add s

-- Function definitions
add :: [Int] -> [Int]
add [x] = [x + 1]

main :: IO ()
main =
  getLine >>= \line ->
    putStrLn . unwords . map show . fromSignal . system . signal . map read . words $ line
