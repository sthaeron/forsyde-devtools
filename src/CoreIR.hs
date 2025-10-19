module CoreIR where

import Data.List (intercalate)
import GHC
import GHC.Core
import GHC.Data.EnumSet as EnumSet
import GHC.Driver.Ppr
import GHC.Paths (libdir)
import GHC.Plugins
import System.FilePath (takeBaseName)
import Text.Printf (printf)

indent :: String -> String
indent = unlines . map ("  " ++) . lines

prettyCoreBind :: CoreBind -> String
prettyCoreBind bind = case bind of
  NonRec b e -> printf "NonRec(%s = \n%s)" (showPprUnsafe b) (indent (prettyCoreExpr e))
  Rec binds -> printf "Rec({\n%s\n})" ((intercalate ", " . map prettyBind) binds)

prettyBind :: (Var, CoreExpr) -> String
prettyBind (b, e) = indent (printf "(%s = %s)" (showPprUnsafe b) (prettyCoreExpr e))

prettyCoreBindList :: [CoreBind] -> String
prettyCoreBindList = intercalate "\n\n" . map prettyCoreBind

prettyCoreExpr :: CoreExpr -> String
prettyCoreExpr expr = case expr of
  Var i -> printf "Var(%s)" (showPprUnsafe i)
  Lit l -> printf "Lit(%s)" (showPprUnsafe l)
  App e a -> printf "App(%s * \n%s)" (prettyCoreExpr e) (prettyCoreExpr a)
  Lam b e -> printf "Lam(%s -> \n%s)" (showPprUnsafe b) (prettyCoreExpr e)
  Type t -> printf "Type(%s)" (showPprUnsafe t)
  Let bind e -> printf "Let(%s in\n%s)" (prettyCoreBind bind) (prettyCoreExpr e)
  Case e b _ alts -> printf "Case(%s of %s {\n%s})" (prettyCoreExpr e) (showPprUnsafe b) (prettyCoreAltList alts)
  Cast e co -> printf "Cast(%s by %s)" (prettyCoreExpr e) (showPprUnsafe co)
  Tick t e -> printf "Tick(%s: \n%s)" (showPprUnsafe t) (prettyCoreExpr e)
  Coercion co -> printf "Coercion(%s)" (showPprUnsafe co)

prettyCoreAlt :: CoreAlt -> String
prettyCoreAlt (Alt con bl e) = indent (printf "Alt(%s: {\n%s}%s)" (prettyCoreAltCon con) (indent (prettyCoreBndrList bl)) (prettyCoreExpr e))

prettyCoreBndrList :: [Var] -> String
prettyCoreBndrList = intercalate ", " . map showPprUnsafe

prettyCoreAltCon :: AltCon -> String
prettyCoreAltCon altCons = case altCons of
  DataAlt d -> printf "DataAlt(%s)" (showPprUnsafe d)
  LitAlt l -> printf "LitAlt(%s)" (showPprUnsafe l)
  DEFAULT -> "DEFAULT"

prettyCoreAltList :: [CoreAlt] -> String
prettyCoreAltList = intercalate ", " . map prettyCoreAlt

compileToCore :: FilePath -> IO CoreProgram
compileToCore filePath = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  setSessionDynFlags $
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
  target <- guessTarget filePath Nothing Nothing
  setTargets [target]
  _ <- load LoadAllTargets
  modSummary <- getModSummary $ mkModuleName (takeBaseName filePath)
  parse <- parseModule modSummary
  typecheck <- typecheckModule parse
  desugar <- desugarModule typecheck
  return $ mg_binds . dm_core_module $ desugar
