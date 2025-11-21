module SDFScheduleSpec (spec) where

import ForSyDeIR
import SDFSchedule
import Test.Hspec

----------------------------------------------------------
-- Example Systems to test the algorithm
----------------------------------------------------------
-- System with single actor and self loop
exampleSystem1 :: IRSystem
exampleSystem1 =
  IRSystem
    (["input"], ["output"])
    [ IRActor "actor_1" Actor22 "add" (["s_in", "s_2"], ["s_out", "s_1"]),
      IRDelay "delay_1" [0] ("s_1", "s_2")
    ]
    [ IRSignal "s_in" ("input", 1) ("actor_1", 1),
      IRSignal "s_1" ("actor_1", 1) ("delay_1", 1),
      IRSignal "s_2" ("delay_1", 1) ("actor_1", 1),
      IRSignal "s_out" ("actor_1", 1) ("output", 1)
    ]
    [ IRFunction "add" Nothing
    ]

-- System with single actor and nothing else
exampleSystem2 :: IRSystem
exampleSystem2 =
  IRSystem
    (["in"], ["out"])
    [ IRActor "actor" Actor11 "add" (["s_in"], ["s_out"])
    ]
    [ IRSignal "s_in" ("in", 1) ("actor", 1),
      IRSignal "s_out" ("actor", 1) ("out", 1)
    ]
    [ IRFunction "add" Nothing
    ]

-- System with two actors, one self loop
exampleSystem3 :: IRSystem
exampleSystem3 =
  IRSystem
    (["input"], ["output"])
    [ IRActor "actor_1" Actor22 "add" (["s_in", "s_2"], ["s_1", "s_3"]),
      IRDelay "delay_1" [0] ("s_1", "s_2"),
      IRActor "actor_2" Actor11 "add" (["s_3"], ["s_out"])
    ]
    [ IRSignal "s_in" ("input", 1) ("actor_1", 1),
      IRSignal "s_1" ("actor_1", 1) ("delay_1", 1),
      IRSignal "s_2" ("delay_1", 1) ("actor_1", 1),
      IRSignal "s_3" ("actor_1", 1) ("actor_2", 1),
      IRSignal "s_out" ("actor_2", 1) ("output", 1)
    ]
    [ IRFunction "add" Nothing
    ]

-- System with multiple inputs
exampleSystem4 :: IRSystem
exampleSystem4 =
  IRSystem
    (["s_ina", "s_inb"], ["s_out"])
    [ IRActor "actor_a" Actor11 "add" (["s_ina"], ["s_1"]),
      IRActor "actor_b" Actor11 "add" (["s_inb"], ["s_2"]),
      IRActor "actor_c" Actor21 "add" (["s_1", "s_4"], ["s_3"]),
      IRActor "actor_d" Actor22 "add" (["s_2", "s_3"], ["s_4_delay", "s_out"]),
      IRDelay "delay" [0] ("s_4_delay", "s_4")
    ]
    [ IRSignal "s_ina" ("s_ina", 1) ("actor_a", 2),
      IRSignal "s_inb" ("s_inb", 1) ("actor_b", 1),
      IRSignal "s_1" ("actor_a", 1) ("actor_c", 2),
      IRSignal "s_2" ("actor_b", 2) ("actor_d", 2),
      IRSignal "s_3" ("actor_c", 1) ("actor_d", 1),
      IRSignal "s_4_delay" ("actor_d", 1) ("delay", 1),
      IRSignal "s_4" ("delay", 1) ("actor_c", 1),
      IRSignal "s_out" ("actor_d", 2) ("s_out", 1)
    ]
    [ IRFunction "add" Nothing
    ]

exampleSystem5 :: IRSystem
exampleSystem5 =
  IRSystem
    (["s_in"], ["s_out"])
    [ IRActor "a" Actor21 "add" (["s_in", "s3"], ["s1"]),
      IRActor "b" Actor11 "add" (["s1"], ["s2"]),
      IRActor "c" Actor12 "add" (["s2"], ["s3_delay", "s_out"]),
      IRDelay "delay" [0, 0, 0, 0, 0, 0] ("s3_delay", "s3")
    ]
    [ IRSignal "s_in" ("s_in", 1) ("a", 2),
      IRSignal "s1" ("a", 1) ("b", 2),
      IRSignal "s2" ("b", 3) ("c", 1),
      IRSignal "s3_delay" ("c", 2) ("delay", 2),
      IRSignal "s3" ("delay", 3) ("a", 3),
      IRSignal "s_out" ("c", 1) ("s_out", 1)
    ]
    [ IRFunction "add" Nothing
    ]

exampleSystem6 :: IRSystem
exampleSystem6 =
  IRSystem
    (["s_in"], ["s_out"])
    [ IRActor "a" Actor12 "add" (["s_in", "s4"], ["s1"]),
      IRActor "b" Actor11 "add" (["s1"], ["s2_delay"]),
      IRActor "c" Actor11 "add" (["s3"], ["s4"]),
      IRActor "d" Actor12 "add" (["s2"], ["s3", "s_out"]),
      IRDelay "delay" [0, 0] ("s2_delay", "s2")
    ]
    [ IRSignal "s_in" ("s_in", 1) ("a", 2),
      IRSignal "s1" ("a", 1) ("b", 4),
      IRSignal "s2_delay" ("b", 1) ("delay", 1),
      IRSignal "s2" ("delay", 2) ("d", 2),
      IRSignal "s3" ("d", 4) ("c", 1),
      IRSignal "s4" ("c", 4) ("a", 2),
      IRSignal "s_out" ("d", 1) ("s_out", 1)
    ]
    [ IRFunction "add" Nothing
    ]

------------------------------------------------------------------------------
-- Test Specifications
--------------------------------------------------------
spec :: SpecWith ()
spec = do
  describe "SDF scheduling examples" $ do
    it "exampleSystem1: System with single actor and self loop" $ do
      let (actualSchedule, actualBuffers, _) = computeScheduleAndBuffers exampleSystem1
      let expectedSchedule = ["actor_1"]
      let expectedBuffers = [("s_in", 1), ("s_out", 1), ("s_1_s_2", 1)]
      (actualSchedule, actualBuffers) `shouldBe` (expectedSchedule, expectedBuffers)

    it "exampleSystem2: System with single actor and nothing else" $ do
      let (actualSchedule, actualBuffers, _) = computeScheduleAndBuffers exampleSystem2
      let expectedSchedule = ["actor"]
      let expectedBuffers = [("s_in", 1), ("s_out", 1)]
      (actualSchedule, actualBuffers) `shouldBe` (expectedSchedule, expectedBuffers)

    it "exampleSystem3: System with two actors, one self loop" $ do
      let (actualSchedule, actualBuffers, _) = computeScheduleAndBuffers exampleSystem3
      let expectedSchedule = ["actor_1", "actor_2"]
      let expectedBuffers = [("s_in", 1), ("s_out", 1), ("s_3", 1), ("s_1_s_2", 1)]
      (actualSchedule, actualBuffers) `shouldBe` (expectedSchedule, expectedBuffers)

    it "exampleSystem4: System with multiple inputs" $ do
      let (actualSchedule, actualBuffers, _) = computeScheduleAndBuffers exampleSystem4
      let expectedSchedule = ["actor_a", "actor_a", "actor_b", "actor_c", "actor_d"]
      let expectedBuffers = [("s_ina", 4), ("s_inb", 1), ("s_out", 2), ("s_1", 2), ("s_2", 2), ("s_3", 1), ("s_4_delay_s_4", 1)]
      (actualSchedule, actualBuffers) `shouldBe` (expectedSchedule, expectedBuffers)

    it "exampleSystem5" $ do
      let (actualSchedule, actualBuffers, _) = computeScheduleAndBuffers exampleSystem5
      let expectedSchedule = ["a", "a", "b", "c", "c", "c"]
      let expectedBuffers = [("s_in", 4), ("s_out", 3), ("s1", 2), ("s2", 3), ("s3_delay_s3", 6)]
      (actualSchedule, actualBuffers) `shouldBe` (expectedSchedule, expectedBuffers)

    it "exampleSystem6" $ do
      let (actualSchedule, actualBuffers, _) = computeScheduleAndBuffers exampleSystem6
      let expectedSchedule = ["d", "c", "a", "a", "c", "a", "a", "b", "c", "a", "a", "c", "a", "a", "b"]
      let expectedBuffers = [("s_in", 16), ("s_out", 1), ("s1", 4), ("s3", 4), ("s4", 4), ("s2_delay_s2", 2)]
      (actualSchedule, actualBuffers) `shouldBe` (expectedSchedule, expectedBuffers)
