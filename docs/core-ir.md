# Core IR

## Tutorials
Videos introducing Core can be found in [GHC Hackathon](https://www.youtube.com/watch?v=EQA69dvkQIk&list=PLBkRCigjPwyeCSD_DFxpd246YIF7_RDDI) and [Into the Core - Squeezing Haskell into Nine Constructors by Simon Peyton Jones ](https://www.youtube.com/watch?v=uR_VzYxvbxg). For thorough information about Core, consult the [core syn type](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/core-syn-type#case-expressions) wiki.

## Core algebraic data type
The top level of our Core IR translation starts from a `CoreProgram` which is a type synonym of a list of `CoreBind`. Each `CoreBind` is a type synonym of a `Bind CoreBndr`. A `Bind CoreBndr` is a specific type synonym of the `Bind` data type used by GHC Core. Core algebraic data types are defined in the code snippet below. 

``` Haskell
type CoreExpr = Expr Var

data Expr b	-- "b" for the type of binders, 
  = Var	  Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]
  | Cast  (Expr b) Coercion
  | Tick  (Tickish Id) (Expr b)
  | Type  Type
  | Coercion Coercion

type Arg b = Expr b
type Alt b = (AltCon, [b], Expr b)

data AltCon = DataAlt DataCon | LitAlt  Literal | DEFAULT

data Bind b = NonRec b (Expr b) | Rec [(b, (Expr b))]
```
## GHC APIs
The GHC APIs version 9.10.2 can be found [here](https://hackage.haskell.org/package/ghc-9.10.2). Below is how the APIs are used in order to transform Haskell code to Core.

```Haskell
compileToCore :: FilePath -> IO (CoreProgram, DynFlags)
compileToCore filePath = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  let newDflags =
        dflags
          { ghcLink = NoLink,
            ghcMode = CompManager,
            backend = interpreterBackend,
            verbosity = 0,
            debugLevel = 0
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

```

The `noInlineTypecheck` is a hand crafted solution to tackle the inlining problem in Core.  discussionThe implementation of this function is inspired by thes in a GitLab [issue](https://gitlab.haskell.org/ghc/ghc/-/issues/24386) from the GHC repo.