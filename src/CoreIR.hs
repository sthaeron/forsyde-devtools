module CoreIR where

import Control.Monad ((<=<))
import GHC
import GHC.Core
import GHC.Core.Ppr
import GHC.Data.EnumSet as EnumSet
import GHC.Driver.DynFlags (initDefaultSDocContext)
import GHC.Driver.Monad
import GHC.Driver.Ppr
import GHC.Driver.Session (defaultFatalMessager, defaultFlushOut)
import GHC.Paths
import GHC.Paths (libdir)
import GHC.Plugins
import GHC.Types.Var
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModGuts
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Utils.Outputable (showSDocUnsafe)
import GHC.Utils.Ppr (Mode (PageMode))
import System.FilePath (takeBaseName)
import System.IO (stdout)
import Text.Printf (printf)

prettyCoreBind :: CoreBind -> String
prettyCoreBind bind = case bind of
  NonRec b e -> printf "NonRec(%s: \n\t%s)" (prettyVar b) (prettyCoreExpr e)
  Rec bindExprList -> printf "Rec({%s})" (prettyBindExprList bindExprList)

prettyCoreBindList :: [CoreBind] -> String
prettyCoreBindList binds = concatMap (\b -> prettyCoreBind b ++ "\n\n") binds

prettyBindExpr :: (Var, CoreExpr) -> String
prettyBindExpr (b, e) = printf "(%s, %s), " (prettyVar b) (prettyCoreExpr e)

prettyBindExprList :: [(Var, CoreExpr)] -> String
prettyBindExprList bexprs = concatMap prettyBindExpr bexprs

prettyCoreExpr :: CoreExpr -> String
prettyCoreExpr expr = case expr of
  Var i -> printf "Var(%s)" (showSDocUnsafe (ppr i))
  Lit l -> printf "Lit(%s)" (showSDocUnsafe (ppr l))
  App e a -> printf "App(%s * \n\t%s)" (prettyCoreExpr e) (prettyCoreExpr a)
  Lam b e -> printf "Lam(%s -> \n\t%s)" (prettyVar b) (prettyCoreExpr e)
  Type t -> printf "Type(%s)" (showSDocUnsafe (ppr t))
  Let bind e -> printf "Let(%s =\n\t %s)" (prettyCoreBind bind) (prettyCoreExpr e)
  Case e b t alts -> printf "Case(%s: (%s, %s) \n\t{%s})" (prettyCoreExpr e) (prettyVar b) (showSDocUnsafe (ppr t)) (prettyCoreAltList alts)
  Cast e co -> printf "Cast(%s: \n\tCoercion(%s))" (prettyCoreExpr e) (showSDocUnsafe (ppr co))
  Tick t e -> printf "Tick(%s: \n\t%s)" (showSDocUnsafe (ppr t)) (prettyCoreExpr e)
  Coercion co -> printf "Coercion(%s)" (showSDocUnsafe (ppr co))

prettyCoreAlt :: CoreAlt -> String
prettyCoreAlt (Alt con bl e) = printf "Alt(%s: \n\t{%s}%s)" (prettyCoreAltCon con) (prettyCoreBndrList bl) (prettyCoreExpr e)

prettyVar :: Var -> String
prettyVar v = showSDocUnsafe (ppr v)

prettyCoreBndrList :: [Var] -> String
prettyCoreBndrList bndrs = concatMap (\x -> printf "%s, " (prettyVar x)) bndrs

prettyCoreAltCon :: AltCon -> String
prettyCoreAltCon altCons = case altCons of
  DataAlt d -> printf "DataAlt(%s)" (showSDocUnsafe (ppr d))
  LitAlt l -> printf "LitAlt(%s)" (showSDocUnsafe (ppr l))
  DEFAULT -> "DEFAULT"

prettyCoreAltList :: [CoreAlt] -> String
prettyCoreAltList alts = concatMap (\x -> printf "%s, " (prettyCoreAlt x)) alts

compileToDesugar :: FilePath -> IO CoreProgram
compileToDesugar filePath = runGhc (Just libdir) $ do
  setSessionDynFlags =<< getSessionDynFlags
  target <- guessTarget filePath Nothing Nothing
  setTargets [target]
  load LoadAllTargets
  ds <- desugarModule <=< typecheckModule <=< parseModule <=< getModSummary $ mkModuleName (takeBaseName filePath)
  return $ mg_binds . dm_core_module $ ds

compileToCore :: FilePath -> IO CoreProgram
compileToCore filePath = runGhc (Just libdir) $ do
  setSessionDynFlags =<< getSessionDynFlags
  target <- guessTarget filePath Nothing Nothing
  setTargets [target]
  load LoadAllTargets
  ds <- desugarModule <=< typecheckModule <=< parseModule <=< getModSummary $ mkModuleName (takeBaseName filePath)
  return $ mg_binds . coreModule $ ds
