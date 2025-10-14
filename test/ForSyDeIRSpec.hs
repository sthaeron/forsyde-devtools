module ForSyDeIRSpec (spec) where

import ForSyDeIR
import GHC.Core
import GHC.Core.Ppr (pprCoreExpr)
import GHC.Utils.Outputable
import Test.Hspec

exampleSystem :: IRSystem
exampleSystem =
  IRSystem
    (["input"], ["output"])
    [ IRActor "actor_1" Actor22 "add",
      IRDelay "delay_1" [0]
    ]
    [ IRSignal "s_in" ("input", 1) ("actor_1", 1),
      IRSignal "s_1" ("actor_1", 1) ("delay_1", 1),
      IRSignal "s_2" ("delay_1", 1) ("actor_1", 1),
      IRSignal "s_out" ("actor_1", 1) ("output", 1)
    ]
    [ IRFunction "add" Nothing
    ]

-- This is in my opinion horrible, but it seems that multiline strings only
-- got added in GHC 9.12...
exampleSystemPretty =
  "IRSystem (({ input } , { output }) ,\n\
  \          { IRActor (actor_1 , Actor22 , add),\n\
  \            IRDelay (delay_1 , [0])\n\
  \          } ,\n\
  \          { IRSignal (s_in , (input , 1) , (actor_1 , 1)),\n\
  \            IRSignal (s_1 , (actor_1 , 1) , (delay_1 , 1)),\n\
  \            IRSignal (s_2 , (delay_1 , 1) , (actor_1 , 1)),\n\
  \            IRSignal (s_out , (actor_1 , 1) , (output , 1))\n\
  \          } ,\n\
  \          { IRFunction (add ,)\n\
  \          })"

testForSyDeIR :: IO ()
testForSyDeIR = do
  putStrLn $ showSDocUnsafe $ prettyIRSystem exampleSystem

spec :: SpecWith ()
spec = do
  describe "IR pretty-printing" $ do
    it "Test hand-crafted IRSystem" $ do
      (showSDocUnsafe $ prettyIRSystem exampleSystem) `shouldBe` exampleSystemPretty
    it "Empty IR-system should not be an empty string" $ do
      (showSDocUnsafe $ prettyIRSystem $ IRSystem ([], []) [] [] []) `shouldNotBe` ""
