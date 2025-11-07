module Utilities (noInlineTypecheck, compileToCore) where

import Data.Data (Data, gmapT)
import Data.Generics (extT)
import GHC
import GHC.Driver.Main
import GHC.Paths (libdir)
import GHC.Plugins
import GHC.Tc.Types
import System.FilePath (takeBaseName)

-- | Custom `compileToCore` function which compiles a haskell module at a
-- specified file path into GHC Core. Returns a `CoreProgram` and the internally
-- defined and used `DynFlags`. This flags are used for safe pretty printing.
compileToCore :: FilePath -> IO (CoreProgram, DynFlags)
compileToCore filePath = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  let newDflags =
        dflags
          { ghcLink = NoLink,
            ghcMode = CompManager,
            backend = interpreterBackend,
            verbosity = 0,
            debugLevel = 1
          }
  _ <- setSessionDynFlags newDflags
  target <- guessTarget filePath Nothing Nothing
  setTargets [target]
  _ <- load LoadAllTargets
  modSummary <- getModSummary $ mkModuleName (takeBaseName filePath)
  env <- getSession
  parsedModule <- liftIO $ hscParse env modSummary
  (tcg, _) <- liftIO $ hscTypecheckRename env modSummary parsedModule
  let noInlineTcg = noInlineTypecheck tcg
  guts <- liftIO $ hscDesugar env modSummary noInlineTcg
  return $ (mg_binds guts, newDflags)

-- | Updates all bindings within the function called `system` and adds NOINLINE
-- pragmas. Prevents the pre optimisier run during desurgaring from inlining
-- bindings relating to variables and functions within the compiled net list.
--
-- Solution is inspired by discussions from the GHC API issue:
-- https://gitlab.haskell.org/ghc/ghc/-/issues/24386
noInlineTypecheck :: TcGblEnv -> TcGblEnv
noInlineTypecheck tcg = tcg {tcg_binds = applySystemBinds (tcg_binds tcg)}
  where
    applySystemBinds :: (Data a) => a -> a
    applySystemBinds = gmapT (applySystemBinds `extT` checkFunBind)

    checkFunBind :: HsBind GhcTc -> HsBind GhcTc
    checkFunBind bind =
      case bind of
        FunBind {fun_id = var, fun_matches = matches} ->
          let funName = occNameString (occName (unLoc var))
           in case funName of
                "system" -> bind {fun_id = var, fun_matches = applyLocalBinds matches}
                _ -> bind
        _ -> applySystemBinds bind

    applyLocalBinds :: (Data a) => a -> a
    applyLocalBinds = gmapT (applyLocalBinds `extT` noInlineBinds)

    noInlineId :: Id -> Id
    noInlineId var = var `setInlinePragma` neverInlinePragma

    noInlineBinds :: HsBind GhcTc -> HsBind GhcTc
    noInlineBinds bind = case bind of
      VarBind {var_id = var, var_rhs = rhs} -> bind {var_id = noInlineId var, var_rhs = applyLocalBinds rhs}
      FunBind {fun_id = var, fun_matches = matches} -> bind {fun_id = noInlineId <$> var, fun_matches = applyLocalBinds matches}
      PatBind {pat_lhs = lhs, pat_rhs = rhs} -> bind {pat_lhs = noInlinePat <$> lhs, pat_rhs = applyLocalBinds rhs}
      XHsBindsLR absBinds -> XHsBindsLR (noInlineAbsBinds absBinds)
      _ -> bind

    noInlinePat :: Pat GhcTc -> Pat GhcTc
    noInlinePat pattern = case pattern of
      VarPat ext var -> VarPat ext (noInlineId <$> var)
      _ -> gmapT (id `extT` noInlinePat) pattern

    noInlineAbsBinds :: AbsBinds -> AbsBinds
    noInlineAbsBinds absBinds@AbsBinds {abs_binds = binds, abs_exports = exports} =
      absBinds {abs_binds = binds, abs_exports = map noInlineABE exports}
      where
        noInlineABE :: ABExport -> ABExport
        noInlineABE abe@ABE {abe_poly = poly, abe_mono = mono} =
          abe {abe_poly = noInlineId poly, abe_mono = noInlineId mono}
