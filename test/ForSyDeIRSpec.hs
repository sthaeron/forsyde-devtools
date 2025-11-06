module ForSyDeIRSpec (spec) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.List (dropWhileEnd)
import ForSyDeIR
import GHC
import GHC.Data.EnumSet as EnumSet
import GHC.Paths (libdir)
import GHC.Plugins
import Test.Hspec

simpleIRSystem :: IRSystem
simpleIRSystem =
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

customDflags :: IO DynFlags
customDflags = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  return $
    updOptLevel 2 $
      dflags
        { ghcLink = NoLink,
          ghcMode = CompManager,
          verbosity = 0,
          debugLevel = 0,
          generalFlags =
            EnumSet.fromList
              [ Opt_SuppressTicks,
                Opt_SuppressCoercions,
                Opt_SuppressCoercionTypes,
                Opt_SuppressVarKinds,
                Opt_SuppressModulePrefixes,
                Opt_SuppressTypeApplications,
                Opt_SuppressIdInfo,
                Opt_SuppressUnfoldings,
                Opt_SuppressTypeSignatures,
                Opt_SuppressUniques,
                Opt_SuppressStgExts,
                Opt_SuppressStgReps,
                Opt_SuppressTimestamps,
                Opt_SuppressCoreSizes
              ]
        }

spec :: SpecWith ()
spec = do
  describe "IR pretty-printing" $ do
    it "Test hand-crafted IRSystem" $ do
      dflags <- customDflags
      simpleIRSystemString <- readFile "examples/test/simple.fir"
      prettyIRSystem dflags simpleIRSystem
        `shouldBe` simpleIRSystemString
    it "Test hand-crafted IRSystem (JSON)" $ do
      simpleIRSystemString <- readFile "examples/test/simple.json"
      encode simpleIRSystem
        `shouldBe` BSC.pack (dropWhileEnd (`elem` "\n") (simpleIRSystemString))
    it "Empty IR-system should not be an empty string" $ do
      dflags <- customDflags
      prettyIRSystem dflags (IRSystem ([], []) [] [] []) `shouldNotBe` ""
