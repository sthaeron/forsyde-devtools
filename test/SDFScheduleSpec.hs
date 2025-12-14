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
    ([IRString "input"], [IRString "output"])
    [ IRActor (IRString "actor_1") Actor22 (IRString "add") ([IRString "s_in", IRString "s_2"], [IRString "s_out", IRString "s_1"]),
      IRDelay (IRString "delay_1") [0] (IRString "s_1", IRString "s_2")
    ]
    [ IRSignal (IRString "s_in") (IRString "input", 1) (IRString "actor_1", 1),
      IRSignal (IRString "s_1") (IRString "actor_1", 1) (IRString "delay_1", 1),
      IRSignal (IRString "s_2") (IRString "delay_1", 1) (IRString "actor_1", 1),
      IRSignal (IRString "s_out") (IRString "actor_1", 1) (IRString "output", 1)
    ]
    [ IRFunction (IRString "add") Nothing
    ]

-- System with single actor and nothing else
exampleSystem2 :: IRSystem
exampleSystem2 =
  IRSystem
    ([IRString "in"], [IRString "out"])
    [ IRActor (IRString "actor") Actor11 (IRString "add") ([IRString "s_in"], [IRString "s_out"])
    ]
    [ IRSignal (IRString "s_in") (IRString "in", 1) (IRString "actor", 1),
      IRSignal (IRString "s_out") (IRString "actor", 1) (IRString "out", 1)
    ]
    [ IRFunction (IRString "add") Nothing
    ]

-- System with two actors, one self loop
exampleSystem3 :: IRSystem
exampleSystem3 =
  IRSystem
    ([IRString "input"], [IRString "output"])
    [ IRActor (IRString "actor_1") Actor22 (IRString "add") ([IRString "s_in", IRString "s_2"], [IRString "s_1", IRString "s_3"]),
      IRDelay (IRString "delay_1") [0] (IRString "s_1", IRString "s_2"),
      IRActor (IRString "actor_2") Actor11 (IRString "add") ([IRString "s_3"], [IRString "s_out"])
    ]
    [ IRSignal (IRString "s_in") (IRString "input", 1) (IRString "actor_1", 1),
      IRSignal (IRString "s_1") (IRString "actor_1", 1) (IRString "delay_1", 1),
      IRSignal (IRString "s_2") (IRString "delay_1", 1) (IRString "actor_1", 1),
      IRSignal (IRString "s_3") (IRString "actor_1", 1) (IRString "actor_2", 1),
      IRSignal (IRString "s_out") (IRString "actor_2", 1) (IRString "output", 1)
    ]
    [ IRFunction (IRString "add") Nothing
    ]

-- System with multiple inputs
exampleSystem4 :: IRSystem
exampleSystem4 =
  IRSystem
    ([IRString "s_ina", IRString "s_inb"], [IRString "s_out"])
    [ IRActor (IRString "actor_a") Actor11 (IRString "add") ([IRString "s_ina"], [IRString "s_1"]),
      IRActor (IRString "actor_b") Actor11 (IRString "add") ([IRString "s_inb"], [IRString "s_2"]),
      IRActor (IRString "actor_c") Actor21 (IRString "add") ([IRString "s_1", IRString "s_4"], [IRString "s_3"]),
      IRActor (IRString "actor_d") Actor22 (IRString "add") ([IRString "s_2", IRString "s_3"], [IRString "s_4_delay", IRString "s_out"]),
      IRDelay (IRString "delay") [0] (IRString "s_4_delay", IRString "s_4")
    ]
    [ IRSignal (IRString "s_ina") (IRString "s_ina", 1) (IRString "actor_a", 2),
      IRSignal (IRString "s_inb") (IRString "s_inb", 1) (IRString "actor_b", 1),
      IRSignal (IRString "s_1") (IRString "actor_a", 1) (IRString "actor_c", 2),
      IRSignal (IRString "s_2") (IRString "actor_b", 2) (IRString "actor_d", 2),
      IRSignal (IRString "s_3") (IRString "actor_c", 1) (IRString "actor_d", 1),
      IRSignal (IRString "s_4_delay") (IRString "actor_d", 1) (IRString "delay", 1),
      IRSignal (IRString "s_4") (IRString "delay", 1) (IRString "actor_c", 1),
      IRSignal (IRString "s_out") (IRString "actor_d", 2) (IRString "s_out", 1)
    ]
    [ IRFunction (IRString "add") Nothing
    ]

exampleSystem5 :: IRSystem
exampleSystem5 =
  IRSystem
    ([IRString "s_in"], [IRString "s_out"])
    [ IRActor (IRString "a") Actor21 (IRString "add") ([IRString "s_in", IRString "s3"], [IRString "s1"]),
      IRActor (IRString "b") Actor11 (IRString "add") ([IRString "s1"], [IRString "s2"]),
      IRActor (IRString "c") Actor12 (IRString "add") ([IRString "s2"], [IRString "s3_delay", IRString "s_out"]),
      IRDelay (IRString "delay") [0, 0, 0, 0, 0, 0] (IRString "s3_delay", IRString "s3")
    ]
    [ IRSignal (IRString "s_in") (IRString "s_in", 1) (IRString "a", 2),
      IRSignal (IRString "s1") (IRString "a", 1) (IRString "b", 2),
      IRSignal (IRString "s2") (IRString "b", 3) (IRString "c", 1),
      IRSignal (IRString "s3_delay") (IRString "c", 2) (IRString "delay", 2),
      IRSignal (IRString "s3") (IRString "delay", 3) (IRString "a", 3),
      IRSignal (IRString "s_out") (IRString "c", 1) (IRString "s_out", 1)
    ]
    [ IRFunction (IRString "add") Nothing
    ]

exampleSystem6 :: IRSystem
exampleSystem6 =
  IRSystem
    ([IRString "s_in"], [IRString "s_out"])
    [ IRActor (IRString "a") Actor12 (IRString "add") ([IRString "s_in", IRString "s4"], [IRString "s1"]),
      IRActor (IRString "b") Actor11 (IRString "add") ([IRString "s1"], [IRString "s2_delay"]),
      IRActor (IRString "c") Actor11 (IRString "add") ([IRString "s3"], [IRString "s4"]),
      IRActor (IRString "d") Actor12 (IRString "add") ([IRString "s2"], [IRString "s3", IRString "s_out"]),
      IRDelay (IRString "delay") [0, 0] (IRString "s2_delay", IRString "s2")
    ]
    [ IRSignal (IRString "s_in") (IRString "s_in", 1) (IRString "a", 2),
      IRSignal (IRString "s1") (IRString "a", 1) (IRString "b", 4),
      IRSignal (IRString "s2_delay") (IRString "b", 1) (IRString "delay", 1),
      IRSignal (IRString "s2") (IRString "delay", 2) (IRString "d", 2),
      IRSignal (IRString "s3") (IRString "d", 4) (IRString "c", 1),
      IRSignal (IRString "s4") (IRString "c", 4) (IRString "a", 2),
      IRSignal (IRString "s_out") (IRString "d", 1) (IRString "s_out", 1)
    ]
    [ IRFunction (IRString "add") Nothing
    ]

------------------------------------------------------------------------------
-- Test Specifications
--------------------------------------------------------
spec :: SpecWith ()
spec = do
  describe "SDF scheduling examples" $ do
    it "exampleSystem1: System with single actor and self loop" $ do
      let (actualSchedule, actualBuffers, _) = computeScheduleAndBuffers exampleSystem1
      let expectedSchedule = [IRString "actor_1"]
      let expectedBuffers = [(IRString "s_in", 1), (IRString "s_out", 1), (IRString "s_1", 1)]
      (actualSchedule, actualBuffers) `shouldBe` (expectedSchedule, expectedBuffers)

    it "exampleSystem2: System with single actor and nothing else" $ do
      let (actualSchedule, actualBuffers, _) = computeScheduleAndBuffers exampleSystem2
      let expectedSchedule = [IRString "actor"]
      let expectedBuffers = [(IRString "s_in", 1), (IRString "s_out", 1)]
      (actualSchedule, actualBuffers) `shouldBe` (expectedSchedule, expectedBuffers)

    it "exampleSystem3: System with two actors, one self loop" $ do
      let (actualSchedule, actualBuffers, _) = computeScheduleAndBuffers exampleSystem3
      let expectedSchedule = [IRString "actor_1", IRString "actor_2"]
      let expectedBuffers = [(IRString "s_in", 1), (IRString "s_out", 1), (IRString "s_3", 1), (IRString "s_1", 1)]
      (actualSchedule, actualBuffers) `shouldBe` (expectedSchedule, expectedBuffers)

    it "exampleSystem4: System with multiple inputs" $ do
      let (actualSchedule, actualBuffers, _) = computeScheduleAndBuffers exampleSystem4
      let expectedSchedule = [IRString "actor_a", IRString "actor_a", IRString "actor_b", IRString "actor_c", IRString "actor_d"]
      let expectedBuffers = [(IRString "s_ina", 4), (IRString "s_inb", 1), (IRString "s_out", 2), (IRString "s_1", 2), (IRString "s_2", 2), (IRString "s_3", 1), (IRString "s_4_delay", 1)]
      (actualSchedule, actualBuffers) `shouldBe` (expectedSchedule, expectedBuffers)

    it "exampleSystem5" $ do
      let (actualSchedule, actualBuffers, _) = computeScheduleAndBuffers exampleSystem5
      let expectedSchedule = [IRString "a", IRString "a", IRString "b", IRString "c", IRString "c", IRString "c"]
      let expectedBuffers = [(IRString "s_in", 4), (IRString "s_out", 3), (IRString "s1", 2), (IRString "s2", 3), (IRString "s3_delay", 6)]
      (actualSchedule, actualBuffers) `shouldBe` (expectedSchedule, expectedBuffers)

    it "exampleSystem6" $ do
      let (actualSchedule, actualBuffers, _) = computeScheduleAndBuffers exampleSystem6
      let expectedSchedule = [IRString "d", IRString "c", IRString "a", IRString "a", IRString "c", IRString "a", IRString "a", IRString "b", IRString "c", IRString "a", IRString "a", IRString "c", IRString "a", IRString "a", IRString "b"]
      let expectedBuffers = [(IRString "s_in", 16), (IRString "s_out", 1), (IRString "s1", 4), (IRString "s3", 4), (IRString "s4", 4), (IRString "s2_delay", 2)]
      (actualSchedule, actualBuffers) `shouldBe` (expectedSchedule, expectedBuffers)
