{-# LANGUAGE DataKinds #-}

module CoreIRToProceduralIR where

import CoreIR (literalToInt, prettyCoreExpr, varToString)
import Data.Maybe (listToMaybe, mapMaybe)
import ForSyDeIR
import GHC hiding (targetId)
import GHC.Core
import ProceduralIR

data TranslationContext = TranslationContext
  { flags :: DynFlags,
    inputSignalIds :: [String],
    outputSignalIds :: [String]
  }

initialTranslationContext :: DynFlags -> TranslationContext
initialTranslationContext dflags =
  TranslationContext
    { flags = dflags,
      inputSignalIds = [],
      outputSignalIds = []
    }

translateIRFunction :: IRFunction -> DynFlags -> [IRConstructor] -> (Maybe Global, Maybe Global)
translateIRFunction function dflags constructors =
  let context = initialTranslationContext dflags
   in case function of
        IRFunction functionId Nothing ->
          let actor = case (findActorFromFunctionId functionId constructors) of
                Nothing -> error ("translateIRFunction - no actor associated to " ++ show functionId)
                Just actor -> actor
              (functionGlobal, _) = getFunctionDeclaration context functionId actor
           in (functionGlobal, Nothing)
        IRFunction functionId (Just functionExpr) ->
          let actor = case (findActorFromFunctionId functionId constructors) of
                Nothing -> error ("translateIRFunction - no actor associated to " ++ show functionId)
                Just actor -> actor
              (functionGlobal, context1) = getFunctionDeclaration context functionId actor
              context2 = translateFunctionExpr context1 functionId functionExpr
           in (functionGlobal, Nothing) -- Placeholder for actual return value

-- (b, Lam _ (Lam _ e))
--   | b == IRString "add" ->
--       let functionScopeStmt = translateCoreExprToStatement context e
--           initFunctionGlobal = GFuncDeclare (Just Static) TVoid (show binder) [(TPointer TInt, "input_1"), (TPointer TInt, "input_2"), (TPointer TInt, "output")]
--           functionGlobal = GFuncDef (Just Static) TVoid (show binder) [(TPointer TInt, "input_1"), (TPointer TInt, "input_2"), (TPointer TInt, "output")] functionScopeStmt
--        in (initFunctionGlobal, functionGlobal)

-- IRActor IRId ActorType IRId ([IRId], [IRId])
getFunctionDeclaration :: TranslationContext -> IRId -> IRConstructor -> (Maybe Global, TranslationContext)
getFunctionDeclaration context functionId (IRActor _ _ _ (inputSignals, outputSignals)) =
  let inputParams = map (\(idx, _) -> (TPointer (TIdent "token"), "input_" ++ show idx)) (zip [1 ..] inputSignals)
      outputParams = map (\(idx, _) -> (TPointer (TIdent "token"), "output_" ++ show idx)) (zip [1 ..] outputSignals)
      allParams = inputParams ++ outputParams
      functionGlobal = GFuncDeclare (Just Static) TVoid (show functionId) allParams
      inputSignalIds = map snd inputParams
      outputSignalIds = map snd outputParams
      context1 = context {inputSignalIds = inputSignalIds, outputSignalIds = outputSignalIds}
   in (Just functionGlobal, context1)
getFunctionDeclaration context _functionId (IRDelay _ _ _) = (Nothing, context)

findActorFromFunctionId :: IRId -> [IRConstructor] -> Maybe IRConstructor
findActorFromFunctionId targetFunctionId constructors =
  let checkIRConstructor :: IRConstructor -> Maybe IRConstructor
      checkIRConstructor actor@(IRActor _ _ functionId _)
        | functionId == targetFunctionId = Just actor
        | otherwise = Nothing
      checkIRConstructor _ = Nothing
   in listToMaybe (mapMaybe checkIRConstructor constructors)

translateFunctionExpr :: TranslationContext -> IRId -> CoreExpr -> TranslationContext
translateFunctionExpr context functionId coreExpr = context

--     = Var	  Id
--   | Lit   Literal
--   | App   (Expr b) (Arg b)
--   | Lam   b (Expr b)
--   | Let   (Bind b) (Expr b)
--   | Case  (Expr b) b Type [Alt b]
--   | Cast  (Expr b) Coercion
--   | Tick  (Tickish Id) (Expr b)
--   | Type  Type
--   | Coercion Coercion

-- type Arg b = Expr b
-- type Alt b = (AltCon, [b], Expr b)

-- data AltCon = DataAlt DataCon | LitAlt  Literal | DEFAULT

-- data Bind b = NonRec b (Expr b) | Rec [(b, (Expr b))]
