module SDF_example_003 where

import ForSyDe.Shallow

-- -- Netlist
system s_in = s_out
  where
    (s_out, s_1) = a_1 s_in s_2
    s_2 = d_1 s_1

-- -- Process specifications

a_1 s_1 s_2 = actor22SDF (1, 1) (1, 1) add s_1 s_2

d_1 s_1 = delaySDF [0] s_1

-- -- Function definitions
add [x] [y] = ([x + y], [x + y])

-- Function definitions
-- add_0 :: Int -> Int
-- add_0 x = x + 1

-- add_1 :: Int -> Int -> Int
-- add_1 x y = x + y

-- add_2 :: Int -> Int -> (Int, Int)
-- add_2 x y = (x + 1, x + y)

-- add_3 :: [Int] -> [Int]
-- add_3 [x] = [x + 1]

-- add_4 :: [Int] -> [Int]
-- add_4 [x, y] = [x + y]

-- add_5 :: [Int] -> [Int] -> [Int]
-- add_5 [x] [y] = [x + y]

-- add_6 :: [Int] -> [Int] -> ([Int], [Int])
-- add_6 [x, y] [z] = ([x + y + z], [x + y, x + y + z])

-- add_7 :: [Int] -> [Int] -> [Int] -> ([Int], [Int])
-- add_7 [x] [y] [z] = ([x + y + z], [x + y, x + y + z])
