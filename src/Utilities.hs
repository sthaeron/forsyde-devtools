module Utilities
  ( compileToCore,
    compileToCoreWithForSyDePath,
    noInlineTypecheck,
    scheduleAndBuffer,
    Stack,
    emptyStack,
    push,
    pop,
    peek,
    isEmpty,
    stackToList,
  )
where

import CoreIRToForSyDeIR (translateCoreProgram)
import Data.Data (Data, gmapT)
import Data.Generics (extT)
import ForSyDeIR (IRId (..))
import GHC
import GHC.Driver.Main
import GHC.Paths (libdir)
import GHC.Plugins hiding (isEmpty)
import GHC.Tc.Types
import SDFSchedule (computeScheduleAndBuffers)
import System.FilePath (takeBaseName)

newtype Stack a = Stack [a] deriving (Show, Eq)

emptyStack :: Stack a
emptyStack = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x : xs)) = Just (x, Stack xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x : _)) = Just x

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty (Stack _) = False

stackToList :: Stack a -> [a]
stackToList (Stack list) = list

-- | Custom `compileToCore` function which compiles a haskell module at a
-- specified file path into GHC Core. Returns a `CoreProgram` and the internally
-- defined and used `DynFlags`. This flags are used for safe pretty printing.
compileToCore :: FilePath -> IO (CoreProgram, DynFlags)
compileToCore = compileToCoreWithForSyDePath Nothing

compileToCoreWithForSyDePath :: Maybe FilePath -> FilePath -> IO (CoreProgram, DynFlags)
compileToCoreWithForSyDePath forSyDePath filePath = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  let newDflags = makeDynFlags dflags
  _ <- setSessionDynFlags $ newDflags
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
  where
    makeDynFlags dflags =
      let newDynFlags =
            dflags
              { ghcLink = NoLink,
                ghcMode = CompManager,
                backend = interpreterBackend,
                verbosity = 0,
                debugLevel = 0
              }
       in case forSyDePath of
            Just path ->
              newDynFlags
                { packageDBFlags = [PackageDB $ PkgDbPath $ path],
                  packageFlags = [ExposePackage "forsyde-shallow" (PackageArg "forsyde-shallow") (ModRenaming True [])]
                }
            Nothing -> newDynFlags

-- | Updates all bindings within the function called `system` and adds NOINLINE
-- pragmas. Prevents the pre optimisier run during desugaring from inlining
-- bindings relating to variables and functions within the compiled netlist.
--
-- Solution is inspired by discussions from the GHC API issue:
-- https://gitlab.haskell.org/ghc/ghc/-/issues/24386
noInlineTypecheck :: TcGblEnv -> TcGblEnv
noInlineTypecheck tcg = tcg {tcg_binds = noInline (tcg_binds tcg)}
  where
    noInline :: (Data a) => a -> a
    noInline =
      gmapT
        ( noInline
            `extT` noInlineBinds
            `extT` noInlinePat
            `extT` noInlineExpr
        )

    noInlineId :: Id -> Id
    noInlineId var = var `setInlinePragma` neverInlinePragma

    noInlineBinds :: HsBind GhcTc -> HsBind GhcTc
    noInlineBinds bind = case bind of
      VarBind {var_id = var, var_rhs = rhs} ->
        bind {var_id = noInlineId var, var_rhs = noInline rhs}
      FunBind {fun_id = var, fun_matches = matches} ->
        bind {fun_id = noInlineId <$> var, fun_matches = noInline matches}
      PatBind {pat_lhs = lhs, pat_rhs = rhs} ->
        bind {pat_lhs = noInline lhs, pat_rhs = noInline rhs}
      XHsBindsLR absBinds ->
        XHsBindsLR (noInlineAbsBinds absBinds)
      _ -> bind

    noInlinePat :: Pat GhcTc -> Pat GhcTc
    noInlinePat pattern = case pattern of
      VarPat ext var -> VarPat ext (noInlineId <$> var)
      _ -> gmapT noInline pattern

    noInlineExpr :: HsExpr GhcTc -> HsExpr GhcTc
    noInlineExpr expr = case expr of
      HsCase x scrutiny matches ->
        HsCase x (noInline scrutiny) (noInline matches)
      HsLet x binds inner ->
        HsLet x (noInline binds) (noInline inner)
      _ -> gmapT noInline expr

    noInlineAbsBinds :: AbsBinds -> AbsBinds
    noInlineAbsBinds absBinds@AbsBinds {abs_binds = binds, abs_exports = exports} =
      absBinds
        { abs_binds = fmap skipFirstHsBind <$> binds,
          abs_exports = map noInlineABE exports
        }
      where
        skipFirstHsBind :: HsBind GhcTc -> HsBind GhcTc
        skipFirstHsBind bind = case bind of
          XHsBindsLR b -> XHsBindsLR (noInlineAbsBinds b)
          _ -> gmapT noInline bind

        noInlineABE :: ABExport -> ABExport
        noInlineABE abe@ABE {abe_poly = poly, abe_mono = mono} =
          abe {abe_poly = noInlineId poly, abe_mono = noInlineId mono}

-- | Utility function to obtain output of SDF Scheduler. Meant for testing to be
-- used in a repl.
scheduleAndBuffer :: FilePath -> IO ([IRId], [(IRId, Int)], [(IRId, IRId)])
scheduleAndBuffer filePath = do
  (core, dflags) <- (compileToCore filePath)
  let (forsydeIR, _lookupSignals) = translateCoreProgram dflags core
  return $ computeScheduleAndBuffers forsydeIR
