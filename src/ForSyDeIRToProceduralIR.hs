module ForSyDeIRToProceduralIR where

import ForSyDeIR
import GHC hiding (targetId)
import GHC.Core
import GHC.Driver.Ppr
import GHC.Types.Literal
import ProceduralIR
import Prelude hiding (id, init)

-- | The `TranslationContext` is a data type which is used to pass around
-- context required to complete the translation of ForSyDeIR to
-- ProceduralIR.
data TranslationContext = TranslationContext
  { flags :: DynFlags, -- Stores `DynFlags` for safely obtaining strings
    lookupSignals :: [(String, IRSignal)],
    actors :: [(String, [Statement])], -- Associated list of actor ids and statements
    signals :: [(String, Statement)], -- Associated list of signal ids and statement
    ioTokens :: [Statement], -- List of statements for input/output tokens
    initBuffers :: [Statement], -- List of statements for initialising signal buffers
    freeBuffers :: [Statement], -- List of statements for freeing signal buffers
    systemInputs :: [String], -- List of system inputs
    systemOutputs :: [String], -- List of system outputs
    delayBuffers :: [(String, String)], -- Associated list of signal ids and buffer name for delay signals
    initDelay :: [Statement], -- List of statements for initialising delay tokens
    initFunctions :: [Global],
    functions :: [Global]
  }

initialTranslationContext :: DynFlags -> [(String, IRSignal)] -> [String] -> [String] -> [(String, String)] -> TranslationContext
initialTranslationContext dflags lookupSignalList inputList outputList delayBufferList =
  TranslationContext
    { flags = dflags,
      lookupSignals = lookupSignalList,
      actors = [],
      signals = [],
      ioTokens = [],
      initBuffers = [],
      freeBuffers = [],
      systemInputs = inputList,
      systemOutputs = outputList,
      delayBuffers = delayBufferList,
      initDelay = [],
      initFunctions = [],
      functions = []
    }

-- | Translates a ForSyDe IR `IRSystem` into a Procedural IR `Program`. Requires
-- the schedule, buffer, and delay buffer lists from the SDF scheduler as an
-- input. Also requires an associated list of signals for lookups.
translateIRSystemToProgram :: DynFlags -> [String] -> [(String, Int)] -> [(String, String)] -> [(String, IRSignal)] -> IRSystem -> Program
translateIRSystemToProgram dflags scheduleList bufferList delayBufferList lookupSignalList (IRSystem (inputList, outputList) constructors _signalList functionList) =
  let initialContext = initialTranslationContext dflags lookupSignalList inputList outputList delayBufferList
      context1 = foldl' translateIRConstructor initialContext constructors
      context2 = foldl' translateBuffer context1 bufferList
      context3 = foldl' translateIRFunctionToGlobals context2 functionList
      main = translateContextToMain context3 scheduleList
   in Prog (reverse (initFunctions context3) ++ [main] ++ reverse (functions context3))

-- | Translates the `TranslationContext` and Schedule into a main function.
translateContextToMain :: TranslationContext -> [String] -> Global
translateContextToMain context scheduleList =
  let scheduledStmts = scheduleActors context scheduleList
      whileStmt = SWhile (EInt 1) (SScope scheduledStmts)
      mainInitStmts = [SExpr (ECall "init" []), SVarDecl TInt "status"] ++ reverse (initBuffers context) ++ reverse (ioTokens context) ++ reverse (initDelay context)
      mainFreeStmts = reverse (freeBuffers context) ++ [SReturn (Just (EInt 0))]
      mainBody = SScope (mainInitStmts ++ [whileStmt] ++ mainFreeStmts)
   in GFuncDef Nothing TInt "main" [] mainBody

-- | Returns a list of statements corresponding to actor function calls based
-- on a provided Schedule. Note that actor function call statements include
-- the associated minimum runtime enviorment statements for interacting with
-- standard in and out.
scheduleActors :: TranslationContext -> [String] -> [Statement]
scheduleActors context scheduleList = foldl' foldSchedule [] scheduleList
  where
    foldSchedule :: [Statement] -> String -> [Statement]
    foldSchedule acc id =
      let actorStmts = case lookup id (actors context) of
            Just stmts -> stmts
            Nothing -> error ("schedule - actor not found: " ++ id)
       in (acc ++ actorStmts)

-- | Translates a component of the buffer size list provided by the SDF
-- scheduler into an io token statment, initialisation statement, and freeing
-- statement which are used to update the `TranslationContext`. Note that io
-- token statements are only created for buffers which relate to with system
-- inputs and outputs.
translateBuffer :: TranslationContext -> (String, Int) -> TranslationContext
translateBuffer initialContext (id, bufferSize) =
  let initStmt = SVarDef (TPointer (TIdent "buffer_nonblocking")) id (ECall "buffer_nonblocking_new" [EInt bufferSize])
      freeStmt = SExpr (ECall "buffer_nonblocking_free" [EVar id])
   in if (elem id (systemInputs initialContext))
        then
          let ioTokenStmt = SArrayDecl TInt ("input_" ++ id) [EInt bufferSize]
           in initialContext {ioTokens = ioTokenStmt : ioTokens initialContext, initBuffers = initStmt : initBuffers initialContext, freeBuffers = freeStmt : freeBuffers initialContext}
        else
          if (elem id (systemOutputs initialContext))
            then
              let ioTokenStmt = SVarDecl TInt ("output_" ++ id)
               in initialContext {ioTokens = ioTokenStmt : ioTokens initialContext, initBuffers = initStmt : initBuffers initialContext, freeBuffers = freeStmt : freeBuffers initialContext}
            else
              initialContext {initBuffers = initStmt : initBuffers initialContext, freeBuffers = freeStmt : freeBuffers initialContext}

-- | Translates an `IRConstructor` into a set of statements which are used to
-- update the `TranslationContext`.
translateIRConstructor :: TranslationContext -> IRConstructor -> TranslationContext
translateIRConstructor initialContext constructor = case constructor of
  IRDelay _ tokens (inputSignal, outputSignal) ->
    -- `IRDelay` results in a list of statements adding initial delay tokens
    -- into a signal buffer.
    let maybeBufferId = lookup outputSignal (delayBuffers initialContext)
     in case maybeBufferId of
          Just bufferId ->
            let foldDelayTokens :: [Statement] -> Int -> [Statement]
                foldDelayTokens acc token =
                  let stmt = SExpr (ECall "write_token" [EVar bufferId, EInt token])
                   in (stmt : acc)
                delayStmts = (foldl' foldDelayTokens [] tokens)
             in initialContext {initDelay = delayStmts ++ initDelay initialContext}
          Nothing -> error ("translateIRConstructor - delay buffer not found for signals: " ++ inputSignal ++ ", " ++ outputSignal)
  IRActor actorId actorType functionId (inputSignals, outputSignals) ->
    -- `IRActor` results in a statement calling an SDF actor function within
    -- source rates, target rates, and a function as arguments.
    let sourceRates = map (getSourceRate initialContext) inputSignals
        targetRates = map (getTargetRate initialContext) outputSignals
        actorName = translateActorType actorType
        translatedInputSignals = map (translateSignalId initialContext) inputSignals
        translatedOutputSignals = map (translateSignalId initialContext) outputSignals
        actorCallStmt =
          SExpr
            ( ECall
                actorName
                ( (map (\rate -> EInt (fromIntegral rate)) sourceRates)
                    ++ (map (\rate -> EInt (fromIntegral rate)) targetRates)
                    ++ (map (\id -> EVar id) translatedInputSignals)
                    ++ (map (\id -> EVar id) translatedOutputSignals)
                    ++ [EVar functionId]
                )
            )
        inputStmts = foldl' foldActorSignals [] inputSignals
        outputStmts = foldl' foldActorSignals [] outputSignals
        stmts = reverse inputStmts ++ [actorCallStmt] ++ reverse outputStmts
        context1 = initialContext {actors = (actorId, stmts) : actors initialContext}
     in context1
  where
    -- Helper function used to update a signal id into its respecitve signal
    -- buffer id. There is only a difference in the translation for signals
    -- which are source and targets of delays.
    translateSignalId :: TranslationContext -> String -> String
    translateSignalId context signalId =
      case lookup signalId (delayBuffers context) of
        Just bufferName -> bufferName
        Nothing -> signalId
    foldActorSignals :: [Statement] -> String -> [Statement]
    foldActorSignals acc id =
      if (elem id (systemInputs initialContext))
        then
          let bufferSize = getSourceRate initialContext id
              -- If actor has inputs which are system inputs the following
              -- adds statements which relate to obtaining inputs from
              -- standard in.
              scanForStmt =
                SFor
                  (SVarDef TInt "i" (EInt 0))
                  (EBinOp Less (EVar "i") (EInt (bufferSize)))
                  (SExpr (EUnOp Increment (EVar "i")))
                  (SScope [SVarAssign "status" (ECall "scanf" [EString "%d", EReference (EArrayAccess (EVar ("input_" ++ id)) (EVar "i"))])])
              breakIfStmt = SIf (EBinOp Less (EVar "status") (EInt 1)) (SBreak) Nothing
              writeForStmt =
                SFor
                  (SVarDef TInt "i" (EInt 0))
                  (EBinOp Less (EVar "i") (EInt (bufferSize)))
                  (SExpr (EUnOp Increment (EVar "i")))
                  (SScope [SExpr (ECall "write_token" [EVar id, EArrayAccess (EVar ("input_" ++ id)) (EVar "i")])])
           in (writeForStmt : breakIfStmt : scanForStmt : acc)
        else
          if (elem id (systemOutputs initialContext))
            then
              let bufferSize = getTargetRate initialContext id
                  -- If actor has outputs which are system outputs the
                  -- following adds statements which relate to printing
                  -- outputs to standard out.
                  scopeStmt =
                    SScope
                      [ SExpr (ECall "read_token" [EVar id, EReference (EVar ("output_" ++ id))]),
                        SExpr (ECall "printf" [EString "%d", EVar ("output_" ++ id)])
                      ]
                  forStmt =
                    SFor
                      (SVarDef TInt "i" (EInt 0))
                      (EBinOp Less (EVar "i") (EInt (bufferSize)))
                      (SExpr (EUnOp Increment (EVar "i")))
                      scopeStmt
                  newLineStmt = SExpr (ECall "printf" [EString "\n"])
               in (newLineStmt : forStmt : acc)
            else
              acc

translateActorType :: ActorType -> String
translateActorType actorType = case actorType of
  Actor11 -> "actor11SDF"
  Actor12 -> "actor12SDF"
  Actor13 -> "actor13SDF"
  Actor14 -> "actor14SDF"
  Actor21 -> "actor21SDF"
  Actor22 -> "actor22SDF"
  Actor23 -> "actor23SDF"
  Actor24 -> "actor24SDF"
  Actor31 -> "actor31SDF"
  Actor32 -> "actor32SDF"
  Actor33 -> "actor33SDF"
  Actor34 -> "actor34SDF"
  Actor41 -> "actor41SDF"
  Actor42 -> "actor42SDF"
  Actor43 -> "actor43SDF"
  Actor44 -> "actor44SDF"

getSourceRate :: TranslationContext -> String -> Int
getSourceRate context id =
  let signal = lookup id (lookupSignals context)
   in case signal of
        Just (IRSignal _ (_, rate) _) -> rate
        Nothing -> error ("getSourceRate - signal not found: " ++ id)

getTargetRate :: TranslationContext -> String -> Int
getTargetRate context id =
  let signal = lookup id (lookupSignals context)
   in case signal of
        Just (IRSignal _ _ (_, rate)) -> rate
        Nothing -> error ("getTargetRate - signal not found: " ++ id)

getSignalById :: String -> [IRSignal] -> IRSignal
getSignalById signalId signalList = case signalList of
  [] -> error ("getSignalById - signal not found: " ++ signalId)
  (s@(IRSignal id _ _)) : tailSignals ->
    if id == signalId
      then s
      else getSignalById signalId tailSignals

translateIRFunctionToGlobals :: TranslationContext -> IRFunction -> TranslationContext
translateIRFunctionToGlobals currentContext (IRFunction id maybeFunction) = case maybeFunction of
  Just function ->
    let (initFunctionGlobal, functionGlobal) = translateCoreExprToGlobals currentContext id function
        context1 = currentContext {initFunctions = initFunctionGlobal : (initFunctions currentContext), functions = functionGlobal : (functions currentContext)}
     in context1
  Nothing -> currentContext

-- The following is a temporary hard coded solution that translates the inputs
-- of the `add` and `accummulate` functions from SDF example 8.
translateCoreExprToGlobals :: TranslationContext -> String -> CoreExpr -> (Global, Global)
translateCoreExprToGlobals context binder expr = case (binder, expr) of
  ("accumulate", Lam _ (Lam _ e)) ->
    let functionScopeStmt = translateCoreExprToStatement context e
        initFunctionGlobal = GFuncDeclare (Just Static) TVoid (binder) [(TPointer TInt, "input_1"), (TPointer TInt, "input_2"), (TPointer TInt, "output_1"), (TPointer TInt, "output_2")]
        functionGlobal = GFuncDef (Just Static) TVoid (binder) [(TPointer TInt, "input_1"), (TPointer TInt, "input_2"), (TPointer TInt, "output_1"), (TPointer TInt, "output_2")] functionScopeStmt
     in (initFunctionGlobal, functionGlobal)
  ("add", Lam _ (Lam _ e)) ->
    let functionScopeStmt = translateCoreExprToStatement context e
        initFunctionGlobal = GFuncDeclare (Just Static) TVoid (binder) [(TPointer TInt, "input_1"), (TPointer TInt, "input_2"), (TPointer TInt, "output")]
        functionGlobal = GFuncDef (Just Static) TVoid (binder) [(TPointer TInt, "input_1"), (TPointer TInt, "input_2"), (TPointer TInt, "output")] functionScopeStmt
     in (initFunctionGlobal, functionGlobal)
  _ -> error ("translateCoreExprToGlobals - unsupported expression:\n" ++ showPpr (flags context) expr)

-- The following is a temporary hard coded solution that translates the
-- contents and outputs of the `add` and `accumulate` functions from SDF
-- example 8.
translateCoreExprToStatement :: TranslationContext -> CoreExpr -> Statement
translateCoreExprToStatement context expr = case expr of
  Var id -> SExpr (EVar (showPpr (flags context) id))
  Lit (LitNumber LitNumInt i) -> SExpr (EInt (fromIntegral i))
  App (App (App (Var _) (Type _)) (App (App (App (App (Var _op1) (Type _)) (Var _)) (Var _a1)) (Var _a2))) (App (Var _) (Type _)) ->
    let stmt = SArrayAssign "output" (EInt 0) Nothing (EBinOp Plus (EArrayAccess (EVar "input_1") (EInt 0)) (EArrayAccess (EVar "input_2") (EInt 0)))
     in SScope ([stmt])
  App (App (App (App (Var _) (Type _)) (Type _)) (App (App (App (Var _) (Type _)) (App (App (App (App (Var _op1) (Type _)) (Var _)) (Var _a1)) (Var _a2))) (App (Var _) (Type _)))) (App (App (App (Var _) (Type _)) (App (App (App (App (Var _op2) (Type _)) (Var _)) (Var _b1)) (Var _b2))) (App (Var _) (Type _))) ->
    let stmt1 = SArrayAssign "output_1" (EInt 0) Nothing (EBinOp Plus (EArrayAccess (EVar "input_1") (EInt 0)) (EArrayAccess (EVar "input_2") (EInt 0)))
        stmt2 = SArrayAssign "output_2" (EInt 0) Nothing (EBinOp Plus (EArrayAccess (EVar "input_1") (EInt 0)) (EArrayAccess (EVar "input_2") (EInt 0)))
     in SScope ([stmt1, stmt2])
  Let _ e -> translateCoreExprToStatement context e
  Case _ _ _ alts -> translateAltsToStatements context alts
  Lam _ e -> translateCoreExprToStatement context e
  App _ e -> translateCoreExprToStatement context e
  Tick _ e -> translateCoreExprToStatement context e
  _ -> error ("translateCoreExprToStatement - unsupported expression:\n" ++ showPpr (flags context) expr)

translateAltsToStatements :: TranslationContext -> [Alt CoreBndr] -> Statement
translateAltsToStatements context alts = case alts of
  [] -> error ""
  (Alt (DataAlt _) _ e) : [] -> translateCoreExprToStatement context e
  _ : altsTail -> translateAltsToStatements context altsTail
