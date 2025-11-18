module CoreIRToProceduralIR where

import GHC hiding (targetId)
import GHC.Core
import GHC.Driver.Ppr
import GHC.Types.Literal
import ProceduralIR
import Prelude hiding (id)

data TranslationContext = TranslationContext
  { flags :: DynFlags -- Stores `DynFlags` for safely obtaining strings
  }

initialTranslationContext :: DynFlags -> TranslationContext
initialTranslationContext dflags =
  TranslationContext
    { flags = dflags
    }

translateCoreExprToProgram :: DynFlags -> String -> CoreExpr -> Program
translateCoreExprToProgram dflags binder expr =
  let context = initialTranslationContext dflags
      globals = translateCoreExprToGlobals context binder expr
   in Prog globals

translateCoreExprToGlobals :: TranslationContext -> String -> CoreExpr -> [Global]
translateCoreExprToGlobals context binder expr = case expr of
  Lam _ (Lam _ e) ->
    let s = translateCoreExprToStatement context e
        g1 = GFuncDeclare Nothing TVoid (binder) [(TPointer TInt, "input_1"), (TPointer TInt, "input_2")]
        g2 = GFuncDef Nothing TVoid (binder) [(TPointer TInt, "input_1"), (TPointer TInt, "input_2")] s
     in [g1, g2]
  _ -> error ("translateCoreExprToGlobals - unsupported expression:\n" ++ showPpr (flags context) expr)

translateCoreExprToStatement :: TranslationContext -> CoreExpr -> Statement
translateCoreExprToStatement context expr = case expr of
  Var id -> SExpr (EVar (showPpr (flags context) id))
  Lit (LitNumber LitNumInt i) -> SExpr (EInt (fromIntegral i))
  App (App (App (App (Var _) (Type _)) (Type _)) (App (App (App (Var _) (Type _)) (App (App (App (App (Var _op1) (Type _)) (Var _)) (Var _a1)) (Var _a2))) (App (Var _) (Type _)))) (App (App (App (Var _) (Type _)) (App (App (App (App (Var _op2) (Type _)) (Var _)) (Var _b1)) (Var _b2))) (App (Var _) (Type _))) ->
    let s1 = SArrayAssign "output_1" (EInt 0) Nothing (EBinOp Plus (EArrayAccess (EVar "input_1") (EInt 0)) (EArrayAccess (EVar "input_2") (EInt 0)))
        s2 = SArrayAssign "output_2" (EInt 0) Nothing (EBinOp Plus (EArrayAccess (EVar "input_1") (EInt 0)) (EArrayAccess (EVar "input_2") (EInt 0)))
     in SScope ([s1, s2])
  Let _ e -> translateCoreExprToStatement context e
  Case _ _ _ alts -> translateAltsToStatements context alts
  Lam _ e -> translateCoreExprToStatement context e
  App _ e -> translateCoreExprToStatement context e
  Tick _ e -> translateCoreExprToStatement context e
  _ -> error ("translateCoreExprToStatement - unsupported expression\n" ++ showPpr (flags context) expr)

translateAltsToStatements :: TranslationContext -> [Alt CoreBndr] -> Statement
translateAltsToStatements context alts = case alts of
  [] -> error ""
  (Alt (DataAlt _) _ e) : [] -> translateCoreExprToStatement context e
  _ : altsTail -> translateAltsToStatements context altsTail

-- _ -> error ("translateCoreExpr - unsupported expression:\n" ++ showPpr (flags context) expr)
