module ForSyDeIRToProceduralIR where

import ForSyDeIR
import GHC hiding (targetId)
import GHC.Core
import GHC.Driver.Ppr
import GHC.Types.Literal
import ProceduralIR
import Prelude hiding (id, init)

data TranslationContext = TranslationContext
  { flags :: DynFlags, -- Stores `DynFlags` for safely obtaining strings
    ir_signals :: [(String, IRSignal)],
    actors :: [(String, [Statement])],
    signals :: [(String, Statement)],
    io_temporary_tokens :: [Statement],
    init_buffers :: [Statement],
    free_buffers :: [Statement],
    inputs :: [String],
    outputs :: [String],
    delayBuffers :: [(String, String)],
    delays :: [Statement]
  }

initialTranslationContext :: DynFlags -> [(String, IRSignal)] -> [String] -> [String] -> [(String, String)] -> TranslationContext
initialTranslationContext dflags lookupSignals inputList outputList delayBufferList =
  TranslationContext
    { flags = dflags,
      ir_signals = lookupSignals,
      actors = [],
      signals = [],
      io_temporary_tokens = [],
      init_buffers = [],
      free_buffers = [],
      inputs = inputList,
      outputs = outputList,
      delayBuffers = delayBufferList,
      delays = []
    }

translateIRSystemToProgram :: DynFlags -> [String] -> [(String, Int)] -> [(String, String)] -> [(String, IRSignal)] -> IRSystem -> Program
translateIRSystemToProgram dflags scheduleList bufferList delayBufferList lookupSignals (IRSystem (systemInputs, systemOutputs) constructors signals functions) =
  let initialContext = initialTranslationContext dflags lookupSignals systemInputs systemOutputs delayBufferList
      context1 = foldl translateIRConstructor initialContext constructors
      context2 = foldl translateBuffer context1 bufferList
      globals = concatMap (translateIRFunctionToGlobals context2) functions
      main = translateContextToMain context2 scheduleList
   in Prog (globals ++ [main])

translateContextToMain :: TranslationContext -> [String] -> Global
translateContextToMain context scheduleList =
  let scheduledStmts = scheduleActors context scheduleList
      whileStmt = SWhile (EInt 1) (SScope scheduledStmts)
      mainInitStmts = init_buffers context ++ io_temporary_tokens context ++ delays context
      mainFreeStmts = free_buffers context ++ [SReturn (Just (EInt 0))]
      mainBody = SScope (mainInitStmts ++ [whileStmt] ++ mainFreeStmts)
   in GFuncDef Nothing TInt "main" [] mainBody

scheduleActors :: TranslationContext -> [String] -> [Statement]
scheduleActors context scheduleList = aux scheduleList []
  where
    aux :: [String] -> [Statement] -> [Statement]
    aux schedule acc = case schedule of
      [] -> acc
      id : idTail ->
        let actorStmts = case lookup id (actors context) of
              Just stmts -> stmts
              Nothing -> error ("schedule - actor not found: " ++ id)
         in aux idTail (acc ++ actorStmts)

translateBuffer :: TranslationContext -> (String, Int) -> TranslationContext
translateBuffer initialContext (id, bufferSize) =
  let init = SVarDef (TPointer (TIdent "buffer_nonblocking")) id (ECall "buffer_nonblocking_new" [EInt bufferSize])
      free = SExpr (ECall "buffer_nonblocking_free" [EVar id])
   in if (elem id (inputs initialContext))
        then
          let io_token = SVarDecl TInt ("input_" ++ id)
           in initialContext {io_temporary_tokens = io_token : io_temporary_tokens initialContext, init_buffers = init : init_buffers initialContext, free_buffers = free : free_buffers initialContext}
        else
          if (elem id (outputs initialContext))
            then
              let io_token = SVarDecl TInt ("output_" ++ id)
               in initialContext {io_temporary_tokens = io_token : io_temporary_tokens initialContext, init_buffers = init : init_buffers initialContext, free_buffers = free : free_buffers initialContext}
            else
              initialContext {init_buffers = init : init_buffers initialContext, free_buffers = free : free_buffers initialContext}

translateIRConstructor :: TranslationContext -> IRConstructor -> TranslationContext
translateIRConstructor initialContext constructor = case constructor of
  IRDelay _ tokens (inputSignal, outputSignal) ->
    let maybeBufferName = lookup outputSignal (delayBuffers initialContext)
     in case maybeBufferName of
          Just bufferName -> let delayStmts = auxDelay initialContext bufferName tokens [] in initialContext {delays = delayStmts ++ delays initialContext}
          Nothing -> error ("translateIRConstructor - delay buffer not found for signals: " ++ inputSignal ++ ", " ++ outputSignal)
  IRActor actorId actorType functionId (inputSignals, outputSignals) ->
    let sourceRates = map (getSourceRate initialContext) inputSignals
        targetRates = map (getTargetRate initialContext) outputSignals
        actorName = translateActorType actorType
        translatedInputSignals = map (translateSignalId initialContext) inputSignals
        translatedOutputSignals = map (translateSignalId initialContext) outputSignals
        s =
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
        inputStmts = auxActor initialContext inputSignals []
        outputStmts = auxActor initialContext outputSignals []
        stmts = inputStmts ++ [s] ++ outputStmts
        context1 = initialContext {actors = (actorId, stmts) : actors initialContext}
     in context1
  where
    translateSignalId :: TranslationContext -> String -> String
    translateSignalId context signalId =
      case lookup signalId (delayBuffers context) of
        Just bufferName -> bufferName
        Nothing -> signalId
    auxActor :: TranslationContext -> [String] -> [Statement] -> [Statement]
    auxActor context signals acc = case signals of
      [] -> reverse acc
      id : idsTail ->
        if (elem id (inputs context))
          then
            let bufferSize = getSourceRate context id
                scopeStmt =
                  SScope
                    [ SExpr (ECall "scanf" [EString "%d", EReference (EVar ("input_" ++ id))]),
                      SExpr (ECall "write_token" [EVar id, EVar ("input_" ++ id)])
                    ]
                forStmt = SFor (SVarDef TInt "i" (EInt 0)) (EBinOp Less (EVar "i") (EInt (bufferSize))) (SExpr (EUnOp Increment (EVar "i"))) scopeStmt
             in auxActor context idsTail (forStmt : acc)
          else
            if (elem id (outputs context))
              then
                let bufferSize = getTargetRate context id
                    scopeStmt =
                      SScope
                        [ SExpr (ECall "read_token" [EVar id, EReference (EVar ("output_" ++ id))]),
                          SExpr (ECall "printf" [EString "%d", EVar ("output_" ++ id)])
                        ]
                    forStmt = SFor (SVarDef TInt "i" (EInt 0)) (EBinOp Less (EVar "i") (EInt (bufferSize))) (SExpr (EUnOp Increment (EVar "i"))) scopeStmt
                 in auxActor context idsTail ((SExpr (ECall "printf" [EString "\n"])) : forStmt : acc)
              else
                auxActor context idsTail acc
    auxDelay :: TranslationContext -> String -> [Int] -> [Statement] -> [Statement]
    auxDelay context id tokens acc = case tokens of
      [] -> reverse acc
      token : tokensTail ->
        let stmt = SExpr (ECall "write_token" [EVar id, EInt token])
         in auxDelay context id tokensTail (stmt : acc)

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
  let signal = lookup id (ir_signals context)
   in case signal of
        Just (IRSignal _ (_, rate) _) -> rate
        Nothing -> error ("getSourceRate - signal not found: " ++ id)

getTargetRate :: TranslationContext -> String -> Int
getTargetRate context id =
  let signal = lookup id (ir_signals context)
   in case signal of
        Just (IRSignal _ _ (_, rate)) -> rate
        Nothing -> error ("getTargetRate - signal not found: " ++ id)

getSignalById :: String -> [IRSignal] -> IRSignal
getSignalById signalId signals = case signals of
  [] -> error ("getSignalById - signal not found: " ++ signalId)
  (s@(IRSignal id _ _)) : tailSignals ->
    if id == signalId
      then s
      else getSignalById signalId tailSignals

translateIRFunctionToGlobals :: TranslationContext -> IRFunction -> [Global]
translateIRFunctionToGlobals context (IRFunction id maybeFunction) = case maybeFunction of
  Just function -> translateCoreExprToGlobals context id function
  Nothing -> []

translateCoreExprToGlobals :: TranslationContext -> String -> CoreExpr -> [Global]
translateCoreExprToGlobals context binder expr = case (binder, expr) of
  ("accumulate", Lam _ (Lam _ e)) ->
    let s = translateCoreExprToStatement context e
        g1 = GFuncDeclare (Just Static) TVoid (binder) [(TPointer TInt, "input_1"), (TPointer TInt, "input_2"), (TPointer TInt, "output_1"), (TPointer TInt, "output_2")]
        g2 = GFuncDef (Just Static) TVoid (binder) [(TPointer TInt, "input_1"), (TPointer TInt, "input_2"), (TPointer TInt, "output_1"), (TPointer TInt, "output_2")] s
     in [g1, g2]
  ("add", Lam _ (Lam _ e)) ->
    let s = translateCoreExprToStatement context e
        g1 = GFuncDeclare (Just Static) TVoid (binder) [(TPointer TInt, "input_1"), (TPointer TInt, "input_2"), (TPointer TInt, "output")]
        g2 = GFuncDef (Just Static) TVoid (binder) [(TPointer TInt, "input_1"), (TPointer TInt, "input_2"), (TPointer TInt, "output")] s
     in [g1, g2]
  _ -> error ("translateCoreExprToGlobals - unsupported expression:\n" ++ showPpr (flags context) expr)

translateCoreExprToStatement :: TranslationContext -> CoreExpr -> Statement
translateCoreExprToStatement context expr = case expr of
  Var id -> SExpr (EVar (showPpr (flags context) id))
  Lit (LitNumber LitNumInt i) -> SExpr (EInt (fromIntegral i))
  App (App (App (Var _) (Type _)) (App (App (App (App (Var _op1) (Type _)) (Var _)) (Var _a1)) (Var _a2))) (App (Var _) (Type _)) ->
    let s1 = SArrayAssign "output" (EInt 0) Nothing (EBinOp Plus (EArrayAccess (EVar "input_1") (EInt 0)) (EArrayAccess (EVar "input_2") (EInt 0)))
     in SScope ([s1])
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
