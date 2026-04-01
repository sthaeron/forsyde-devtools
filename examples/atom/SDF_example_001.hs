module SDF_example_001 where

import qualified ForSyDe.Atom.MoC.SDF as SDF

system :: SDF.Signal Int -> SDF.Signal Int -> SDF.Signal Int
system s_in_x s_in_y = s_out
  where
    s_1 = a_a s_in_x s_in_y
    (s_out, s_2) = a_b s_1 s_2_delayed
    s_2_delayed = d_1 s_2

-- Process specifications
a_a :: SDF.Signal Int -> SDF.Signal Int -> SDF.Signal Int
a_a s_1 s_2 = SDF.actor21 ((1, 1), 1, add) s_1 s_2

a_b :: SDF.Signal Int -> SDF.Signal Int -> (SDF.Signal Int, SDF.Signal Int)
a_b s_1 s_2 = SDF.actor22 ((1, 1), (1, 1), accumulate) s_1 s_2

d_1 :: SDF.Signal Int -> SDF.Signal Int
d_1 s = SDF.delay [0] s

-- Function definitions
add :: [Int] -> [Int] -> [Int]
add [x] [y] = [x + y]

accumulate :: [Int] -> [Int] -> ([Int], [Int])
accumulate [x] [y] = ([x + y], [x + y])
