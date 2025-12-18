module ForSyDeIRToProceduralIR where

import ArgumentsMain (InputType (Predefined, StdIn), Runs (Limited, Perpetual))
import CoreIRToProceduralIR (translateIRFunction)
import ForSyDeIR
import GHC hiding (targetId)
import ProceduralIR

-- | The `TranslationContext` is a data type which is used to pass around
-- context required to complete the translation of ForSyDeIR to
-- ProceduralIR.
data TranslationContext = TranslationContext
  { flags :: DynFlags, -- Stores `DynFlags` for safely obtaining strings
    inputType :: InputType, -- Type of input handling for the program (stin or predefined)
    runs :: Runs, -- In case of predefined input, how many times to run the SDF model
    lookupSignals :: [(IRId, IRSignal)],
    actors :: [(IRId, [Statement])], -- Associated list of actor ids and statements
    signals :: [(IRId, Statement)], -- Associated list of signal ids and statement
    ioTokens :: [Statement], -- List of statements for input/output tokens
    initBuffers :: [Statement], -- List of statements for initialising signal buffers
    freeBuffers :: [Statement], -- List of statements for freeing signal buffers
    schedulerBufferList :: [(IRId, Int)], -- The buffer list that comes from the schedule
    systemInputs :: [IRId], -- List of system inputs
    systemOutputs :: [IRId], -- List of system outputs
    delayBuffers :: [(IRId, IRId)], -- Associated list of signal ids and buffer name for delay signals
    initDelay :: [Statement], -- List of statements for initialising delay tokens
    initFunctions :: [Global],
    functions :: [Global]
  }

initialTranslationContext :: DynFlags -> InputType -> Runs -> [(IRId, IRSignal)] -> [(IRId, Int)] -> [IRId] -> [IRId] -> [(IRId, IRId)] -> TranslationContext
initialTranslationContext dflags input r lookupSignalList bList inputList outputList delayBufferList =
  TranslationContext
    { flags = dflags,
      inputType = input,
      runs = r,
      lookupSignals = lookupSignalList,
      actors = [],
      signals = [],
      ioTokens = [],
      initBuffers = [],
      freeBuffers = [],
      schedulerBufferList = bList,
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
translateIRSystemToProgram :: DynFlags -> [IRId] -> [(IRId, Int)] -> [(IRId, IRId)] -> [(IRId, IRSignal)] -> InputType -> Runs -> IRSystem -> Program
translateIRSystemToProgram dflags scheduleList bufferList delayBufferList lookupSignalList input r (IRSystem (inputList, outputList) constructors _signalList functionList) =
  let initialContext = initialTranslationContext dflags input r lookupSignalList bufferList inputList outputList delayBufferList
      context1 = foldl' translateIRConstructor initialContext constructors
      context2 = foldl' translateBuffer context1 bufferList
      context3 = foldl' (translateIRFunctionToGlobals constructors) context2 functionList
      main = translateContextToMain context3 scheduleList
   in Prog (reverse (initFunctions context3) ++ [main] ++ reverse (functions context3))

-- | Translates the `TranslationContext` and Schedule into a main function.
translateContextToMain :: TranslationContext -> [IRId] -> Global
translateContextToMain context scheduleList =
  let scheduledStmts = scheduleActors context scheduleList
      scheduledInputStmts = case ((inputType context), (runs context)) of
        (StdIn, _) -> []
        (Predefined, Perpetual) ->
          [SVarAssign "iteration_current" (EBinOp Plus (EVar "iteration_current") (EInt 1))]
            ++ (foldl' resetIterationVariablesFromInputs [] (systemInputs context))
            ++ [ SIf (EBinOp Equal (EVar "iteration_current") (EVar "iteration_max")) (SScope [SVarAssign "iteration_current" (EInt 0)]) Nothing
               ]
        (Predefined, Limited _) ->
          [SVarAssign "iteration_current" (EBinOp Plus (EVar "iteration_current") (EInt 1))]
            ++ (foldl' resetIterationVariablesFromInputs [] (systemInputs context))
            ++ [ SIf
                   (EBinOp Equal (EVar "iteration_current") (EVar "iteration_max"))
                   ( SScope
                       [ SVarAssign "iteration_current" (EInt 0),
                         SVarAssign
                           "run_current"
                           (EBinOp Plus (EVar "run_current") (EInt 1))
                       ]
                   )
                   Nothing
               ]
        where
          resetIterationVariablesFromInputs acc x = SVarAssign ("i_" ++ show x) (EInt (0)) : acc
      scheduledRunsStmts = case ((inputType context), (runs context)) of
        (StdIn, _) -> []
        (Predefined, Perpetual) -> []
        (Predefined, (Limited _)) -> [SIf (EBinOp Equal (EVar "run_current") (EVar "run_max")) (SScope [SBreak]) Nothing]
      whileStmt = SWhile (EInt 1) (SScope (scheduledStmts ++ scheduledInputStmts ++ scheduledRunsStmts))
      mainInitInputStmts = case (inputType context) of
        StdIn -> [SExpr (ECall "init" []), SVarDecl TInt "status"]
        Predefined -> [SExpr (ECall "init" []), SVarDef TInt "iteration_current" (EInt (0))]
      mainInitRunsStmts = case ((inputType context), (runs context)) of
        (StdIn, _) -> []
        (Predefined, Perpetual) -> []
        (Predefined, Limited x) -> [SVarDef TInt "run_current" (EInt (0)), SVarDef TInt "run_max" (EInt (x))]
      mainInitIterStmts = case (inputType context) of
        StdIn -> []
        Predefined -> foldl' defineIterationVariablesFromInputs [] (systemInputs context)
        where
          defineIterationVariablesFromInputs acc x = SVarDef TInt ("i_" ++ show x) (EInt (0)) : acc
      mainInitStmts =
        mainInitInputStmts
          ++ mainInitIterStmts
          ++ mainInitRunsStmts
          ++ reverse (initBuffers context)
          ++ reverse (ioTokens context)
          ++ reverse (initDelay context)
      mainFreeStmts = reverse (freeBuffers context) ++ [SReturn (Just (EInt 0))]
      mainBody = SScope (mainInitStmts ++ [whileStmt] ++ mainFreeStmts)
   in GFuncDef Nothing TInt "main" [] mainBody

-- | Returns a list of statements corresponding to actor function calls based
-- on a provided Schedule. Note that actor function call statements include
-- the associated minimum runtime enviorment statements for interacting with
-- standard in and out.
scheduleActors :: TranslationContext -> [IRId] -> [Statement]
scheduleActors context scheduleList = foldl' foldSchedule [] scheduleList
  where
    foldSchedule :: [Statement] -> IRId -> [Statement]
    foldSchedule acc actorId =
      let actorStmts = case lookup actorId (actors context) of
            Just stmts -> stmts
            Nothing -> error ("schedule - actor not found: " ++ show actorId)
       in (acc ++ actorStmts)

-- | Translates a component of the buffer size list provided by the SDF
-- scheduler into an io token statment, initialisation statement, and freeing
-- statement which are used to update the `TranslationContext`. Note that io
-- token statements are only created for buffers which relate to with system
-- inputs and outputs.
translateBuffer :: TranslationContext -> (IRId, Int) -> TranslationContext
translateBuffer initialContext (bufferId, bufferSize) =
  let initStmt = SVarDef (TPointer (TIdent "buffer_nonblocking")) (show bufferId) (ECall "buffer_nonblocking_new" [EInt bufferSize])
      freeStmt = SExpr (ECall "buffer_nonblocking_free" [EVar $ show bufferId])
   in if (elem bufferId (systemInputs initialContext))
        then case (inputType initialContext) of
          -- If stdin is used, then need to define input handling arrays
          StdIn ->
            let ioTokenStmt = SArrayDecl TInt ("input_" ++ show bufferId) [EInt bufferSize]
             in initialContext {ioTokens = ioTokenStmt : ioTokens initialContext, initBuffers = initStmt : initBuffers initialContext, freeBuffers = freeStmt : freeBuffers initialContext}
          -- If input is predefined, then do not create "input_" arrays.
          -- But you still need to create the "s_" buffers for those signals
          Predefined ->
            initialContext {ioTokens = ioTokens initialContext, initBuffers = initStmt : initBuffers initialContext, freeBuffers = freeStmt : freeBuffers initialContext}
        else
          if (elem bufferId (systemOutputs initialContext))
            then
              let ioTokenStmt = SVarDecl TInt ("output_" ++ show bufferId)
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
                  let stmt =
                        if elem bufferId (systemOutputs initialContext)
                          -- TODO: Match the output with the actor on the signal
                          then SExpr (ECall "printf" [EString "%d\n", EInt token])
                          else SExpr (ECall "write_token" [EVar $ show bufferId, EInt token])
                   in (stmt : acc)
                delayStmts = (foldl' foldDelayTokens [] tokens)
             in initialContext {initDelay = delayStmts ++ initDelay initialContext}
          Nothing -> error ("translateIRConstructor - delay buffer not found for signals: " ++ show inputSignal ++ ", " ++ show outputSignal)
  IRActor actorId actorType functionId (inputSignals, outputSignals) ->
    -- `IRActor` results in a statement calling an SDF actor function within
    -- source rates, target rates, and a function as arguments.
    let inputRates = map (getTargetRate initialContext) inputSignals
        outputRates = map (getSourceRate initialContext) outputSignals
        actorName = translateActorType actorType
        translatedInputSignals = map (translateSignalId initialContext) inputSignals
        translatedOutputSignals = map (translateSignalId initialContext) outputSignals
        actorCallStmt =
          SExpr
            ( ECall
                actorName
                ( (map (\rate -> EInt (fromIntegral rate)) inputRates)
                    ++ (map (\rate -> EInt (fromIntegral rate)) outputRates)
                    ++ (map (\bufferId -> EVar $ show bufferId) translatedInputSignals)
                    ++ (map (\bufferId -> EVar $ show bufferId) translatedOutputSignals)
                    ++ [EVar $ show functionId]
                )
            )
        inputStmts = foldl' foldActorSignals [] translatedInputSignals
        outputStmts = foldl' foldActorSignals [] translatedOutputSignals
        stmts = reverse inputStmts ++ [actorCallStmt] ++ reverse outputStmts
        context1 = initialContext {actors = (actorId, stmts) : actors initialContext}
     in context1
  where
    -- Helper function used to update a signal id into its respecitve signal
    -- buffer id. There is only a difference in the translation for signals
    -- which are source and targets of delays.
    translateSignalId :: TranslationContext -> IRId -> IRId
    translateSignalId context signalId =
      case lookup signalId (delayBuffers context) of
        Just bufferId -> bufferId
        Nothing -> signalId
    foldActorSignals :: [Statement] -> IRId -> [Statement]
    foldActorSignals acc signalId =
      if (elem signalId (systemInputs initialContext))
        then case (inputType initialContext) of
          -- If input type is stdin, then do scanf->break->write
          StdIn ->
            let bufferSize = getTargetRate initialContext signalId
                -- If actor has inputs which are system inputs the following
                -- adds statements which relate to obtaining inputs from
                -- standard in.
                scanForStmt =
                  SFor
                    (SVarDef TInt "i" (ECall "contained_tokens" [EVar $ show signalId]))
                    (EBinOp Less (EVar "i") (EInt (bufferSize)))
                    (SExpr (EUnOp Increment (EVar "i")))
                    (SScope [SVarAssign "status" (ECall "scanf" [EString "%d", EReference (EArrayAccess (EVar ("input_" ++ show signalId)) (EVar "i"))])])
                breakIfStmt = SIf (EBinOp Less (EVar "status") (EInt 1)) (SScope [SBreak]) Nothing
                writeForStmt =
                  SFor
                    (SVarDef TInt "i" (EInt 0))
                    (EBinOp Less (EVar "i") (EInt (bufferSize)))
                    (SExpr (EUnOp Increment (EVar "i")))
                    (SScope [SExpr (ECall "write_token" [EVar $ show signalId, EArrayAccess (EVar ("input_" ++ show signalId)) (EVar "i")])])
             in (writeForStmt : breakIfStmt : scanForStmt : acc)
          -- If input type is predefined, then do write
          Predefined ->
            let bufferSize = getTargetRate initialContext signalId
                writeForStmt =
                  SFor
                    (SVarDef TInt "i" (ECall "contained_tokens" [EVar $ show signalId]))
                    (EBinOp Less (EVar "i") (EInt (bufferSize)))
                    (SExpr (EUnOp Increment (EVar "i")))
                    ( SScope
                        ( [ SExpr
                              ( ECall
                                  "write_token"
                                  [ EVar $ show signalId,
                                    EArrayAccess
                                      (EVar ("input_" ++ show signalId))
                                      ( EBinOp
                                          Plus
                                          ( EBinOp
                                              Multiply
                                              (EVar ("iteration_current"))
                                              (EInt (getScheduleBufferRate signalId (schedulerBufferList initialContext)))
                                          )
                                          (EVar ("i_" ++ show signalId))
                                      )
                                  ]
                              )
                          ]
                            ++ [SVarAssign ("i_" ++ show signalId) (EBinOp Plus (EVar ("i_" ++ show signalId)) (EInt 1))]
                        )
                    )
             in (writeForStmt : acc)
        else
          if (elem signalId (systemOutputs initialContext))
            then
              let bufferSize = getSourceRate initialContext signalId
                  -- If actor has outputs which are system outputs the
                  -- following adds statements which relate to printing
                  -- outputs to standard out.
                  scopeStmt =
                    SScope
                      [ SExpr (ECall "read_token" [EVar $ show signalId, EReference (EVar ("output_" ++ show signalId))]),
                        SExpr (ECall "printf" [EString "%d ", EVar ("output_" ++ show signalId)])
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

getSourceRate :: TranslationContext -> IRId -> Int
getSourceRate context signalId =
  let signal = lookup signalId (lookupSignals context)
   in case signal of
        Just (IRSignal _ (_, rate) _) -> rate
        Nothing -> error ("getSourceRate - signal not found: " ++ show signalId)

getTargetRate :: TranslationContext -> IRId -> Int
getTargetRate context signalId =
  let signal = lookup signalId (lookupSignals context)
   in case signal of
        Just (IRSignal _ _ (_, rate)) -> rate
        Nothing -> error ("getTargetRate - signal not found: " ++ show signalId)

-- Get the rate from the schedule given an ActorId and bufferList
getScheduleBufferRate :: IRId -> [(IRId, Int)] -> Int
getScheduleBufferRate actorId bufferList =
  case bufferList of
    [] -> error ("buffer rate finder - actor not found: " ++ show actorId)
    (x, rate) : xs ->
      if (x == actorId)
        then
          rate
        else
          getScheduleBufferRate actorId xs

translateIRFunctionToGlobals :: [IRConstructor] -> TranslationContext -> IRFunction -> TranslationContext
translateIRFunctionToGlobals constructors currentContext function =
  let context1 = case (translateIRFunction function (flags currentContext) constructors) of
        Just (functionDeclaration, Just functionDefinition) -> currentContext {initFunctions = functionDeclaration : (initFunctions currentContext), functions = functionDefinition : (functions currentContext)}
        Just (functionDeclaration, Nothing) -> currentContext {initFunctions = functionDeclaration : (initFunctions currentContext)}
        Nothing -> currentContext
   in context1
