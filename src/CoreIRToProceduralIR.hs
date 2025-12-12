{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CoreIRToProceduralIR where

import CoreIR (literalToInt, prettyCoreAlt, varToString)
import Data.List (elemIndex)
import Data.Maybe (listToMaybe, mapMaybe)
import ForSyDeIR
import GHC hiding (Type, targetId)
import GHC.Core
import GHC.Driver.Ppr (showPpr)
import GHC.Plugins (Var)
import ProceduralIR
import Utilities (Stack, emptyStack, pop, push, stackToList)

-- | The `TranslationContext` is a data type which is used to pass around
-- context required to complete the translation of Core IR to Procedural IR.
data TranslationContext = TranslationContext
  { flags :: DynFlags, -- Stores `DynFlags` for safely obtaining strings
    inputIds :: [String], -- List of input signal names
    outputIds :: [String], -- List of output signal names
    functionInputs :: [IRId], -- List of function inputs ids
    functionVariables :: [(IRId, Expression)], -- Associated list of functionIds and expression
    scope :: Statement -- Scope of function definition
  }

-- | The `FunctionContent` data type is used to represent the individual
-- components that make up the contents of a function expression using Core IR.
data FunctionContent
  = FVar Var
  | FInt Int
  | FCons
  | FTuple
  | FEmptyList
  | FPlus
  | FMinus
  | FMultiply
  | FDiv
  | FNegate

initialTranslationContext :: DynFlags -> TranslationContext
initialTranslationContext dflags =
  TranslationContext
    { flags = dflags,
      inputIds = [],
      outputIds = [],
      functionInputs = [],
      functionVariables = [],
      scope = SScope []
    }

-- | Translates an `IRFunction` into a Procedural IR function declaration and
-- optional definition.
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

-- Helper function which identifies which `IRActor` is associated with a
-- provided function id. If successful returns the actor as an optional
-- `IRConstructor` otherwise returns `Nothing`.
findActorFromFunctionId :: IRId -> [IRConstructor] -> Maybe IRConstructor
findActorFromFunctionId targetFunctionId constructors =
  let checkIRConstructor :: IRConstructor -> Maybe IRConstructor
      checkIRConstructor actor@(IRActor _ _ functionId _)
        | functionId == targetFunctionId = Just actor
        | otherwise = Nothing
      checkIRConstructor _ = Nothing
   in listToMaybe (mapMaybe checkIRConstructor constructors)

-- Helper function which builds the parameter list for a `ProceduralIR` function
-- based on the input and output signals of provided actor. Updates the
-- `TranslationContext` with the input and output ids.
getParameterList :: TranslationContext -> IRConstructor -> ([(Type, String)], TranslationContext)
getParameterList context (IRActor _ _ _ (inputSignals, outputSignals)) =
  let inputParameters = map (\(index, _) -> (TPointer (TIdent "token"), "input_" ++ show index)) (zip [1 :: Int ..] inputSignals)
      outputParameters = map (\(index, _) -> (TPointer (TIdent "token"), "output_" ++ show index)) (zip [1 :: Int ..] outputSignals)
      parametersList = inputParameters ++ outputParameters
      inputIdList = map snd inputParameters
      outputIdList = map snd outputParameters
      context1 = context {inputIds = inputIdList, outputIds = outputIdList}
   in (parametersList, context1)
getParameterList context (IRDelay _ _ _) = ([], context)

getFunctionDeclaration :: [(Type, String)] -> IRId -> Global
getFunctionDeclaration parameterList functionId =
  let functionDeclareGlobal = GFuncDeclare (Just Static) TVoid (show functionId) parameterList
   in functionDeclareGlobal

getFunctionDefinition :: TranslationContext -> [(Type, String)] -> IRId -> Global
getFunctionDefinition context parametersList functionId =
  let stmtScope = scope context
      functionDefinitionGlobal = GFuncDef (Just Static) TVoid (show functionId) parametersList stmtScope
   in functionDefinitionGlobal

-- | Translates the `CoreExpr` associated with an `IRFunction` into a Procedural
-- IR scope. The input id argument represents the current input being pattern
-- matched. The input index argument represents index of the input the pattern
-- matched binder represents.
translateFunctionExpr :: TranslationContext -> Int -> Int -> CoreExpr -> TranslationContext
translateFunctionExpr context inputId inputIndex expr = case expr of
  -- Function with 4 inputs
  Lam b1 (Lam b2 (Lam b3 (Lam b4 e))) ->
    let context1 = context {functionInputs = [(IRVar b1), (IRVar b2), (IRVar b3), (IRVar b4)]}
     in translateFunctionExpr context1 inputId inputIndex e
  -- Function with 3 inputs
  Lam b1 (Lam b2 (Lam b3 e)) ->
    let context1 = context {functionInputs = [(IRVar b1), (IRVar b2), (IRVar b3)]}
     in translateFunctionExpr context1 inputId inputIndex e
  -- Function with 2 inputs
  Lam b1 (Lam b2 e) ->
    let context1 = context {functionInputs = [(IRVar b1), (IRVar b2)]}
     in translateFunctionExpr context1 inputId inputIndex e
  -- Function with 1 input
  Lam b e ->
    let context1 = context {functionInputs = [(IRVar b)]}
     in translateFunctionExpr context1 inputId inputIndex e
  Let _ e -> translateFunctionExpr context inputId inputIndex e
  -- The Core IR `Case` data type is used in function expressions to pattern
  -- match on a functions inputs. Checks if the current binder being pattern
  -- matched is a function input and get its index.
  Case (Var i) _ _ty alts -> case elemIndex (IRVar i) (functionInputs context) of
    -- The index of an input in the function inputs list is used to id the input
    -- in Procedural IR.
    Just newInputId ->
      -- Pattern matching on a new function input pass new input id and reset
      -- index to zero
      let context1 = translateAltList context alts newInputId 0
       in context1
    Nothing ->
      -- Still pattern maching on previous function input continue passing
      -- current input id and increment input index
      let context1 = translateAltList context alts inputId (inputIndex + 1)
       in context1
  -- Reached the main content of the function
  _ ->
    let functionContentList = translateOutputExprToFunctionContentList context expr []
        scopeStmt = translateFunctionContent context functionContentList
        context1 = context {scope = scopeStmt}
     in context1

-- | Helper function which identifies a binder as a function input and updates
-- the translation context with its associated Procedural IR expression
translateAltList :: TranslationContext -> [Alt CoreBndr] -> Int -> Int -> TranslationContext
translateAltList context alts inputId inputIndex = case alts of
  [] -> context
  (Alt (DataAlt dc) ids e) : altsTail -> case showPpr (flags context) dc of
    -- An empty list pattern match indicates the end of function input
    "[]" ->
      let context1 = translateFunctionExpr context inputId inputIndex e
       in translateAltList context1 altsTail inputId inputIndex
    -- A cons pattern match indicates a new function input, the following
    -- builds the associated variable expression for the input and updates the
    -- translation context
    ":" ->
      let variableId = IRVar (ids !! 0)
          variableExpression = (EArrayAccess (EVar ("input_" ++ show (inputId + 1))) (EInt inputIndex))
          context1 = context {functionVariables = (variableId, variableExpression) : functionVariables context}
          context2 = translateFunctionExpr context1 inputId inputIndex e
       in translateAltList context2 altsTail inputId inputIndex
    _ -> error ("translateAltList - unsupported data alt: " ++ showPpr (flags context) dc)
  -- Skip DEFAULT pattern match, ignoring base case since not translating errors
  (Alt DEFAULT _ _) : altsTail -> translateAltList context altsTail inputId inputIndex
  alt : _ -> error ("translateAltList - unsupported alt: " ++ prettyCoreAlt (flags context) alt)

-- | Translates the main `CoreExpr` of an `IRFunction` into a list of
-- `FunctionContent`. The returned `FunctionContent` list is in reverse polish
-- notation.
translateOutputExprToFunctionContentList :: TranslationContext -> CoreExpr -> [FunctionContent] -> [FunctionContent]
translateOutputExprToFunctionContentList context expr acc = case expr of
  App e a ->
    let acc1 = translateOutputExprToFunctionContentList context e acc
     in translateOutputExprToFunctionContentList context a acc1
  (Var varId) -> case (showPpr (flags context)) varId of
    ":" -> FCons : acc
    "(,)" -> FTuple : acc
    "(,,)" -> FTuple : acc
    "(,,,)" -> FTuple : acc
    "[]" -> FEmptyList : acc
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

-- | Translates a `FunctionContent` list into a Procedural IR scope.
translateFunctionContent :: TranslationContext -> [FunctionContent] -> Statement
translateFunctionContent context contentList =
  let finalExprsStack = foldl' functionContentToStack emptyStack contentList
      stmtList = foldl' stackToScope [] (zip (outputIds context) (stackToList finalExprsStack))
   in SScope stmtList
  where
    -- Helper function which translates an associated output signal id and a
    -- list of expressions into a list of statements. Each statement in the list
    -- represents an array assign of the output at an incrementing index equal
    -- to an expression.
    stackToScope :: [Statement] -> (String, [Expression]) -> [Statement]
    stackToScope acc (outputSignal, exprList) =
      let outputStmts = map (\(idx, expr) -> SArrayAssign outputSignal (EInt idx) Nothing expr) (zip [0 ..] exprList)
       in (acc ++ outputStmts)
    -- Helper function which wraps Procedural IR expression which use non
    -- commutative binary or unary operators in parenthesis.
    wrapNonCommutativeExpression :: Expression -> Expression
    wrapNonCommutativeExpression e = case e of
      EBinOp Minus _ _ -> EParen e
      EBinOp Divide _ _ -> EParen e
      EUnOp Negate _ -> EParen e
      _ -> e
    -- Helper function to handle the case when the `FunctionContent` represents
    -- a `BinaryOperator`
    binOpContentToStack :: BinaryOperator -> Stack [Expression] -> Stack [Expression]
    binOpContentToStack binOp exprsStack =
      let (exprList, exprsStack1) = case (pop exprsStack) of
            Nothing -> error "translateFunctionContent - empty expression stack when popping"
            Just (elist, stack1) -> (elist, stack1)
          (expr1, expr2, exprTail) = case exprList of
            (e1 : e2 : eTail) -> (wrapNonCommutativeExpression e1, wrapNonCommutativeExpression e2, eTail)
            _ -> error ("translateFunctionContent - insufficient operands for " ++ show binOp)
          newExpr = EBinOp binOp expr1 expr2
          newExprList = newExpr : exprTail
          exprsStack2 = push newExprList exprsStack1
       in exprsStack2
    -- Help function which uses a stack to convert a `FunctionContent` the
    -- Procedural IR `Expression` it represents
    functionContentToStack :: (Stack [Expression]) -> FunctionContent -> (Stack [Expression])
    functionContentToStack exprsStack content = case content of
      FEmptyList ->
        let newExprList = []
            exprsStack1 = push newExprList exprsStack
         in exprsStack1
      (FInt i) ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when popping"
              Just (elist, stack1) -> (elist, stack1)
            expr = EInt i
            newExprList = expr : exprList
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      (FVar varId) ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when popping"
              Just (elist, stack1) -> (elist, stack1)
            variableExpr = case lookup (IRVar varId) (functionVariables context) of
              Just expr -> expr
              Nothing -> error ("translateFunctionContent - variable not found: " ++ varToString varId)
            newExprList = variableExpr : exprList
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      FPlus -> binOpContentToStack Plus exprsStack
      FMinus -> binOpContentToStack Minus exprsStack
      FMultiply -> binOpContentToStack Multiply exprsStack
      FDiv -> binOpContentToStack Divide exprsStack
      FNegate ->
        let (exprList, exprsStack1) = case (pop exprsStack) of
              Nothing -> error "translateFunctionContent - empty expression stack when popping"
              Just (elist, stack1) -> (elist, stack1)
            (expr1, exprTail) = case exprList of
              (e1 : eTail) -> (e1, eTail)
              _ -> error "translateFunctionContent - insufficient operands for Negate"
            newExpr = EUnOp Negate expr1
            newExprList = newExpr : exprTail
            exprsStack2 = push newExprList exprsStack1
         in exprsStack2
      FCons -> exprsStack
      FTuple -> exprsStack
