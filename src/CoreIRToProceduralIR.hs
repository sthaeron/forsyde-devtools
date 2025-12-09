{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CoreIRToProceduralIR where

import CoreIR (literalToInt, prettyCoreAlt, varToString)
import Data.List (elemIndex)
import Data.Maybe (listToMaybe, mapMaybe)
import ForSyDeIR
import GHC hiding (targetId)
import GHC.Core
import GHC.Driver.Ppr (showPpr)
import GHC.Plugins (Var)
import ProceduralIR
import Utilities (Stack, emptyStack, pop, push, stackToList)

data TranslationContext = TranslationContext
  { flags :: DynFlags,
    inputSignalIds :: [String],
    outputSignalIds :: [String],
    functionInputs :: [IRId],
    functionVariables :: [(IRId, Expression)],
    scope :: Statement
  }

initialTranslationContext :: DynFlags -> TranslationContext
initialTranslationContext dflags =
  TranslationContext
    { flags = dflags,
      inputSignalIds = [],
      outputSignalIds = [],
      functionInputs = [],
      functionVariables = [],
      scope = SScope []
    }

data FunctionContent
  = FVar Var
  | FInt Int
  | FColon
  | FTuple
  | FList
  | FPlus
  | FMinus
  | FMultiply
  | FDiv
  | FNegate

translateIRFunction :: IRFunction -> DynFlags -> [IRConstructor] -> (Global, Maybe Global)
translateIRFunction function dflags constructors =
  let context = initialTranslationContext dflags
   in case function of
        IRFunction functionId Nothing ->
          let actor = case (findActorFromFunctionId functionId constructors) of
                Nothing -> error ("translateIRFunction - no actor associated to " ++ show functionId)
                Just a -> a
              (parameterList, _context1) = getParameterList context actor
              (functionDeclarationGlobal) = getFunctionDeclaration parameterList functionId
           in (functionDeclarationGlobal, Nothing)
        IRFunction functionId (Just functionExpr) ->
          let actor = case (findActorFromFunctionId functionId constructors) of
                Nothing -> error ("translateIRFunction - no actor associated to " ++ show functionId)
                Just a -> a
              (parameterList, context1) = getParameterList context actor
              functionDeclarationGlobal = getFunctionDeclaration parameterList functionId
              context2 = translateFunctionExpr context1 0 0 functionExpr
              functionDefinitionGlobal = getFunctionDefinition context2 parameterList functionId
           in (functionDeclarationGlobal, Just functionDefinitionGlobal)

getParameterList :: TranslationContext -> IRConstructor -> ([(ProceduralIR.Type, String)], TranslationContext)
getParameterList context (IRActor _ _ _ (inputSignals, outputSignals)) =
  let inputParameters = map (\(idx, _) -> (TPointer (TIdent "token"), "input_" ++ show idx)) (zip [1 :: Int ..] inputSignals)
      outputParameters = map (\(idx, _) -> (TPointer (TIdent "token"), "output_" ++ show idx)) (zip [1 :: Int ..] outputSignals)
      parametersList = inputParameters ++ outputParameters
      inputSignalList = map snd inputParameters
      outputSignalList = map snd outputParameters
      context1 = context {inputSignalIds = inputSignalList, outputSignalIds = outputSignalList}
   in (parametersList, context1)
getParameterList context (IRDelay _ _ _) = ([], context)

getFunctionDeclaration :: [(ProceduralIR.Type, String)] -> IRId -> Global
getFunctionDeclaration parameterList functionId =
  let functionDeclareGlobal = GFuncDeclare (Just Static) TVoid (show functionId) parameterList
   in functionDeclareGlobal

getFunctionDefinition :: TranslationContext -> [(ProceduralIR.Type, String)] -> IRId -> Global
getFunctionDefinition context parametersList functionId =
  let stmtScope = scope context
      functionDefinitionGlobal = GFuncDef (Just Static) TVoid (show functionId) parametersList stmtScope
   in functionDefinitionGlobal

findActorFromFunctionId :: IRId -> [IRConstructor] -> Maybe IRConstructor
findActorFromFunctionId targetFunctionId constructors =
  let checkIRConstructor :: IRConstructor -> Maybe IRConstructor
      checkIRConstructor actor@(IRActor _ _ functionId _)
        | functionId == targetFunctionId = Just actor
        | otherwise = Nothing
      checkIRConstructor _ = Nothing
   in listToMaybe (mapMaybe checkIRConstructor constructors)

translateFunctionExpr :: TranslationContext -> Int -> Int -> CoreExpr -> TranslationContext
translateFunctionExpr context inputIndex inputCounter expr = case expr of
  -- Function with 4 inputs
  Lam b1 (Lam b2 (Lam b3 (Lam b4 e))) ->
    let context1 = context {functionInputs = [(IRVar b1), (IRVar b2), (IRVar b3), (IRVar b4)]}
     in translateFunctionExpr context1 inputIndex inputCounter e
  -- Function with 3 inputs
  Lam b1 (Lam b2 (Lam b3 e)) ->
    let context1 = context {functionInputs = [(IRVar b1), (IRVar b2), (IRVar b3)]}
     in translateFunctionExpr context1 inputIndex inputCounter e
  -- Function with 2 inputs
  Lam b1 (Lam b2 e) ->
    let context1 = context {functionInputs = [(IRVar b1), (IRVar b2)]}
     in translateFunctionExpr context1 inputIndex inputCounter e
  -- Function with 1 input
  Lam b e ->
    let context1 = context {functionInputs = [(IRVar b)]}
     in translateFunctionExpr context1 inputIndex inputCounter e
  Let _ e -> translateFunctionExpr context inputIndex inputCounter e
  Case (Var i) _ _ty alts -> case elemIndex (IRVar i) (functionInputs context) of
    Just newInputIndex ->
      let context1 = translateAltList context alts newInputIndex 0
       in context1
    Nothing ->
      let context1 = translateAltList context alts inputIndex inputCounter
       in context1
  _ ->
    let functionContentList = translateOutputExprToFunctionContentList context expr []
        scopeStmt = translateFunctionContent context functionContentList
        context1 = context {scope = scopeStmt}
     in context1

translateFunctionContent :: TranslationContext -> [FunctionContent] -> Statement
translateFunctionContent context contentList =
  let finalExprsStack = foldl' functionContentToStack emptyStack contentList
      stmtList = foldl' stackToScope [] (zip (outputSignalIds context) (stackToList finalExprsStack))
   in SScope stmtList
  where
    stackToScope :: [Statement] -> (String, [Expression]) -> [Statement]
    stackToScope acc (outputSignal, exprList) =
      let outputStmts = map (\(idx, expr) -> SArrayAssign outputSignal (EInt idx) Nothing expr) (zip [0 ..] exprList)
       in (acc ++ outputStmts)
    functionContentToStack :: (Stack [Expression]) -> FunctionContent -> (Stack [Expression])
    functionContentToStack exprsStack content = case content of
      FList ->
        let newExprList = []
            exprsStack1 = push newExprList exprsStack
         in exprsStack1
      (FInt i) ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when expecting expression list"
              Just (elist, stack1) -> (elist, stack1)
            expr = EInt i
            newExprList = expr : exprList
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      (FVar varId) ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when expecting expression list"
              Just (elist, stack1) -> (elist, stack1)
            variableExpr = case lookup (IRVar varId) (functionVariables context) of
              Just expr -> expr
              Nothing -> error ("translateFunctionContent - variable not found: " ++ varToString varId)
            newExprList = variableExpr : exprList
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      FPlus ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when expecting expression list"
              Just (elist, stack1) -> (elist, stack1)
            (expr1, expr2, exprTail) = case exprList of
              (e1@(EBinOp _ _ _) : e2@(EBinOp _ _ _) : eTail) -> (EParen e1, EParen e2, eTail)
              (e1@(EBinOp _ _ _) : e2 : eTail) -> (EParen e1, e2, eTail)
              (e1 : e2@(EBinOp _ _ _) : eTail) -> (e1, EParen e2, eTail)
              (e1 : e2 : eTail) -> (e1, e2, eTail)
              _ -> error "translateFunctionContent - insufficient operands for Plus"
            newExpr = EBinOp Plus expr1 expr2
            newExprList = newExpr : exprTail
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      FMinus ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when expecting expression list"
              Just (elist, stack1) -> (elist, stack1)
            (expr1, expr2, exprTail) = case exprList of
              (e1@(EBinOp _ _ _) : e2@(EBinOp _ _ _) : eTail) -> (EParen e1, EParen e2, eTail)
              (e1@(EBinOp _ _ _) : e2 : eTail) -> (EParen e1, e2, eTail)
              (e1 : e2@(EBinOp _ _ _) : eTail) -> (e1, EParen e2, eTail)
              (e1 : e2 : eTail) -> (e1, e2, eTail)
              _ -> error "translateFunctionContent - insufficient operands for Minus"
            newExpr = EBinOp Minus expr1 expr2
            newExprList = newExpr : exprTail
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      FMultiply ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when expecting expression list"
              Just (elist, stack1) -> (elist, stack1)
            (expr1, expr2, exprTail) = case exprList of
              (e1@(EBinOp _ _ _) : e2@(EBinOp _ _ _) : eTail) -> (EParen e1, EParen e2, eTail)
              (e1@(EBinOp _ _ _) : e2 : eTail) -> (EParen e1, e2, eTail)
              (e1 : e2@(EBinOp _ _ _) : eTail) -> (e1, EParen e2, eTail)
              (e1 : e2 : eTail) -> (e1, e2, eTail)
              _ -> error "translateFunctionContent - insufficient operands for Multiply"
            newExpr = EBinOp Multiply expr1 expr2
            newExprList = newExpr : exprTail
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      FDiv ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when expecting expression list"
              Just (elist, stack1) -> (elist, stack1)
            (expr1, expr2, exprTail) = case exprList of
              (e1@(EBinOp _ _ _) : e2@(EBinOp _ _ _) : eTail) -> (EParen e1, EParen e2, eTail)
              (e1@(EBinOp _ _ _) : e2 : eTail) -> (EParen e1, e2, eTail)
              (e1 : e2@(EBinOp _ _ _) : eTail) -> (e1, EParen e2, eTail)
              (e1 : e2 : eTail) -> (e1, e2, eTail)
              _ -> error "translateFunctionContent - insufficient operands for Div"
            newExpr = EBinOp Divide expr1 expr2
            newExprList = newExpr : exprTail
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      FNegate ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when expecting expression list"
              Just (elist, stack1) -> (elist, stack1)
            (expr1, exprTail) = case exprList of
              (e1 : eTail) -> (e1, eTail)
              _ -> error "translateFunctionContent - insufficient operands for Negate"
            newExpr = EParen (EUnOp Negate expr1)
            newExprList = newExpr : exprTail
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      FColon -> exprsStack
      FTuple -> exprsStack

translateOutputExprToFunctionContentList :: TranslationContext -> CoreExpr -> [FunctionContent] -> [FunctionContent]
translateOutputExprToFunctionContentList context expr acc = case expr of
  App e a ->
    let acc1 = translateOutputExprToFunctionContentList context e acc
     in translateOutputExprToFunctionContentList context a acc1
  (Var varId) -> case (showPpr (flags context)) varId of
    ":" -> FColon : acc
    "(,)" -> FTuple : acc
    "(,,)" -> FTuple : acc
    "(,,,)" -> FTuple : acc
    "[]" -> FList : acc
    "I#" -> acc
    "$fNumInt" -> acc
    "$fIntegralInt" -> acc
    "+" -> FPlus : acc
    "-" -> FMinus : acc
    "*" -> FMultiply : acc
    "div" -> FDiv : acc
    "negate" -> FNegate : acc
    _ -> FVar varId : acc
  Lit i -> FInt (literalToInt i) : acc
  _ -> acc

translateAltList :: TranslationContext -> [Alt CoreBndr] -> Int -> Int -> TranslationContext
translateAltList context alts inputIndex inputCounter = case alts of
  [] -> context
  (Alt (DataAlt dc) ids e) : altsTail -> case showPpr (flags context) dc of
    "[]" ->
      let context1 = translateFunctionExpr context inputIndex 0 e
       in translateAltList context1 altsTail inputIndex inputCounter
    ":" ->
      let variableId = IRVar (ids !! 0)
          variableExpression = (EArrayAccess (EVar ("input_" ++ show (inputIndex + 1))) (EInt inputCounter))
          context1 = context {functionVariables = (variableId, variableExpression) : functionVariables context}
          context2 = translateFunctionExpr context1 inputIndex (inputCounter + 1) e
       in translateAltList context2 altsTail inputIndex inputCounter
    _ -> error ("translateAltList - unsupported data alt: " ++ showPpr (flags context) dc)
  (Alt DEFAULT _ _) : altsTail -> translateAltList context altsTail inputIndex inputCounter
  alt : _ -> error ("translateAltList - unsupported alt: " ++ prettyCoreAlt (flags context) alt)
