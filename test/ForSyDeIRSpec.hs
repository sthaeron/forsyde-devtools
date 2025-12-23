module ForSyDeIRSpec (spec) where

import ForSyDeIR
import GHC
import GHC.Data.EnumSet as EnumSet
import GHC.Paths (libdir)
import GHC.Plugins
import Test.Hspec

simpleIRSystem :: IRSystem
simpleIRSystem =
  IRSystem
    ([IRString "input"], [IRString "output"])
    [ IRActor (IRString "actor_1") Actor22 (IRFunction (IRString "add") Nothing) ([IRString "s_in", IRString "s_2"], [IRString "s_out", IRString "s_1"]),
      IRDelay (IRString "delay_1") [0] (IRString "s_1", IRString "s_2")
    ]
    [ IRSignal (IRString "s_in") (IRString "input", 1) (IRString "actor_1", 1),
      IRSignal (IRString "s_1") (IRString "actor_1", 1) (IRString "delay_1", 1),
      IRSignal (IRString "s_2") (IRString "delay_1", 1) (IRString "actor_1", 1),
      IRSignal (IRString "s_out") (IRString "actor_1", 1) (IRString "output", 1)
    ]
    [ IRFunction (IRString "add") Nothing
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
      prettyIRJSON simpleIRSystem
        `shouldBe` simpleIRSystemString
    it "Empty IR-system should not be an empty string" $ do
      dflags <- customDflags
      prettyIRSystem dflags (IRSystem ([], []) [] [] []) `shouldNotBe` ""
