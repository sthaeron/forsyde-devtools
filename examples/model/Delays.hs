module Delays where

import ForSyDe.Shallow

d_1 s = delaySDF [0] s

d_2 s = delaySDF [0, 1] s

d_3 s = delaySDF [0, 1, 2] s
