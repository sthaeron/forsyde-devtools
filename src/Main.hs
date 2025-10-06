module Main where

import GHC
import GHC.Driver.Ppr
import GHC.Paths
import GHC.Utils.Outputable
import GHC.Driver.Monad
import GHC.Data.EnumSet ( fromList )

import GHC.Driver.Session ( defaultFatalMessager, defaultFlushOut )

main = 
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      coreIR <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags { 
            ghcLink = LinkInMemory,
            ghcMode = CompManager,
            generalFlags = fromList [
                Opt_SuppressTicks,
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
        compileToCoreModule "examples/model/SDF_example_002.hs"
      putStrLn $ showSDocUnsafe $ ppr coreIR
