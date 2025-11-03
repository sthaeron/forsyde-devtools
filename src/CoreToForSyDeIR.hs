{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CoreToForSyDeIR where

import CoreIR
import Data.List (elemIndex)
import ForSyDeIR
import GHC
import GHC.Core
import GHC.Driver.Ppr
import GHC.Types.Literal
import Prelude hiding (id)

data TranslationContext = TranslationContext
  { flags :: DynFlags,
    constructors :: [(String, IRConstructor)],
    signals :: [(String, IRSignal)],
    functions :: [(String, IRFunction)],
    systemInputs :: [String],
    systemOutputs :: [String],
    nameCounter :: Int,
    actors :: [(String, ([Int], [Int]))] -- [(actorId, (inputRates, outputRates))]
    -- binds [(binder, prId)] ???
  }

initialTranslationContext :: DynFlags -> TranslationContext
initialTranslationContext dflags =
  TranslationContext
    { flags = dflags,
      constructors = [],
      signals = [],
      functions = [],
      systemInputs = [],
      systemOutputs = [],
      nameCounter = 0,
      actors = [] -- (actorIds, (inputRates, outputRates))
    }

translateCoreProgram :: DynFlags -> [CoreBind] -> IRSystem
translateCoreProgram dflags binds =
  let finalContext = finaliseConstructors (foldl translateCoreBind (initialTranslationContext dflags) binds)
   in IRSystem
        (systemInputs finalContext, systemOutputs finalContext)
        (map snd (constructors finalContext))
        (map snd (signals finalContext))
        (map snd (functions finalContext))

finaliseConstructors :: TranslationContext -> TranslationContext
finaliseConstructors context = context {constructors = aux (map snd (signals context)) (constructors context)}
  where
    aux :: [IRSignal] -> [(String, IRConstructor)] -> [(String, IRConstructor)]
    aux currentSignals currentConstructors = case currentSignals of
      [] -> currentConstructors
      (IRSignal signalId (sourceId, _) (targetId, _)) : signalTail ->
        let newConstructors = map (updateConstructor signalId sourceId targetId) currentConstructors
         in aux signalTail newConstructors
    updateConstructor :: String -> String -> String -> (String, IRConstructor) -> (String, IRConstructor)
    updateConstructor signalId sourceId targetId (constructorId, constructor) = case constructor of
      IRDelay prId tokens (inputSignal, outputSignal) ->
        if prId == sourceId
          then (constructorId, IRDelay prId tokens (inputSignal, signalId))
          else
            if prId == targetId
              then (constructorId, IRDelay prId tokens (signalId, outputSignal))
              else (constructorId, constructor)
      IRActor prId actorType functionId (inputSignals, outputSignals) ->
        if prId == sourceId
          then (constructorId, IRActor prId actorType functionId (inputSignals, signalId : outputSignals))
          else
            if prId == targetId
              then (constructorId, IRActor prId actorType functionId (signalId : inputSignals, outputSignals))
              else (constructorId, constructor)

translateCoreBind :: TranslationContext -> CoreBind -> TranslationContext
translateCoreBind context (NonRec b e) = case (showPpr (flags context) b) of
  "$trModule" -> context
  "system" -> translateSystem context e
  _ -> translateCoreExpr context b e
translateCoreBind context (Rec _) = context

translateSystem :: TranslationContext -> CoreExpr -> TranslationContext
translateSystem context expr = case expr of
  Lam _ (Lam _ e) -> translateInputs context e
  _ -> context

translateInputs :: TranslationContext -> CoreExpr -> TranslationContext
translateInputs context expr = case expr of
  Lam b e ->
    let newInput = showPpr (flags context) b
        newContext = context {systemInputs = newInput : (systemInputs context)}
     in translateInputs newContext e
  Let (Rec binds) out ->
    let newContext = translateOutputs context out
        (prBinds, newNewContext) = translateBinds newContext binds -- pr_binds = [(ds, a_1)] -1 ?
     in updateSignals newNewContext prBinds
  Let (NonRec b e) out ->
    let newContext = translateOutputs context out -- [(binder, output)] [(ds, s_out1), (ds, s_out2)]
        binder = showPpr (flags newContext) b -- ds
        (prId, newNewNewContext) = translateBodyExpr newContext [] e -- prId = a_1
     in updateSignals newNewNewContext [(binder, prId)]
  _ -> error ("TranslateInputs: unsupported expression\n" ++ prettyCoreExpr (flags context) expr)

updateSignals :: TranslationContext -> [(String, String)] -> TranslationContext
updateSignals context binds =
  let newSignals = map (updateSignal) (signals context)
   in context {signals = newSignals}
  where
    updateSignal :: (String, IRSignal) -> (String, IRSignal)
    updateSignal (currentSignalId, currentSignal) = case currentSignal of
      IRSignal signalId (sourceId, sourceRate) (targetId, targetRate) ->
        case findBinder sourceId binds of
          Nothing -> case findBinder targetId binds of
            Nothing -> (currentSignalId, currentSignal)
            Just prId ->
              let inputRates = case (findRates prId (actors context)) of
                    Just (rates, _) -> rates
                    Nothing -> error ("No rates found for actor: " ++ prId)
                  newSignal = IRSignal signalId (sourceId, sourceRate) (prId, inputRates !! targetRate)
               in (currentSignalId, newSignal)
          Just prId ->
            let outputRates = case (findRates prId (actors context)) of
                  Just (_, rates) -> rates
                  Nothing -> error ("No rates found for actor: " ++ prId)
                newSignal = IRSignal signalId (prId, outputRates !! sourceRate) (targetId, targetRate)
             in (currentSignalId, newSignal)

findBinder :: String -> [(String, String)] -> Maybe String
findBinder targetBinder binds = case binds of
  [] -> Nothing
  (binder, prId) : bindTail ->
    if targetBinder == binder
      then Just prId
      else findBinder targetBinder bindTail

createSignalsFromBinds :: TranslationContext -> [(String, String)] -> [(String, String)] -> TranslationContext
createSignalsFromBinds context bindPr binds = aux1 context bindPr binds
  where
    aux1 currentContext currentBindPr currentBinds = case currentBindPr of
      [] -> currentContext
      (currentBinder, currentPrId) : bindPrTail ->
        let outputRates = case (findRates currentPrId (actors currentContext)) of
              Just (_, rates) -> rates
              Nothing -> error ("No rates found for actor: " ++ currentPrId)
            newContext = aux2 currentContext (currentBinder, currentPrId) currentBinds outputRates
         in aux1 newContext bindPrTail currentBinds
    aux2 currentContext (currentBinder, currentPrId) currentBinds currentRates = case (currentBinds, currentRates) of
      ([], _) -> currentContext
      (_, []) -> currentContext
      ((sourceBinder, targetId) : binderTail, currentRatesHead : currentRatesTail) ->
        if currentBinder == sourceBinder
          then
            let (name, newContext) = (genSignalName currentContext)
                newSignal = IRSignal name (currentPrId, currentRatesHead) (targetId, 1)
                newNewContext = newContext {signals = (name, newSignal) : (signals newContext)}
             in aux2 newNewContext (currentBinder, currentPrId) binderTail currentRatesTail
          else aux2 currentContext (currentBinder, currentPrId) binderTail (currentRatesHead : currentRatesTail)

translateOutputs :: TranslationContext -> CoreExpr -> TranslationContext
translateOutputs context expr = case expr of
  App e a -> translateOutputs (translateOutputs context a) e
  Var _ -> context
  Type _ -> context
  Case e _ _ alts -> case getVarFromAlts context alts of
    (_, Nothing) -> context
    (binder, Just index) ->
      -- s_out, 0
      let bind = showPpr (flags context) e -- ds
          (name, newContext) = genSignalName context
          newSignal = IRSignal name (bind, index) (binder, 1)
       in newContext {systemOutputs = binder : (systemOutputs newContext), signals = (name, newSignal) : (signals newContext)}
  _ -> error ("translateOutputs: unsupported expression\n" ++ prettyCoreExpr (flags context) expr)

-- Returns chosen variable from a AltCon as a string
getVarFromAlts :: TranslationContext -> [Alt CoreBndr] -> (String, Maybe Int)
getVarFromAlts context alts = case alts of
  [] -> error ("getIndexFromAlts: empty AltCon list")
  (Alt _ binds (Var (i))) : [] ->
    let binder = showPpr (flags context) i
        index = elemIndex (i) (binds)
     in (binder, index)
  _ -> error ("getIndexFromAlts: more than one AltCon\n" ++ prettyCoreAltList (flags context) alts)

translateBinds :: TranslationContext -> [(CoreBndr, CoreExpr)] -> ([(String, String)], TranslationContext)
translateBinds context binds = aux context binds []
  where
    aux currentContext currentBinds acc = case currentBinds of
      [] -> (acc, currentContext)
      (binder, expr) : bindTail ->
        let outputId = showPpr (flags currentContext) binder
            (prId, newContext) = translateBodyExpr currentContext [] expr
         in aux newContext bindTail ((outputId, prId) : acc)

translateBodyExpr :: TranslationContext -> [(String, Maybe Int)] -> CoreExpr -> (String, TranslationContext)
translateBodyExpr context arguments expr = case expr of
  App (App (Var i) _) _ ->
    let prId = showPpr (flags context) i
        newContext = createSignals context prId arguments
     in (prId, newContext)
  App e a ->
    let (newArguments, newContext) = translateArgument context arguments a
     in translateBodyExpr newContext newArguments e
  _ -> error ("translateBodyExpr: unsupported expression\n" ++ prettyCoreExpr (flags context) expr)

-- Returns a list of arguments
translateArgument :: TranslationContext -> [(String, Maybe Int)] -> CoreExpr -> ([(String, Maybe Int)], TranslationContext)
translateArgument context arguments expr = case expr of
  Var i -> let id = showPpr (flags context) i in ((id, Nothing) : arguments, context)
  App e a ->
    let (newArguments, newContext) = translateArgument context [] a
        (prId, newNewContext) = translateBodyExpr newContext newArguments e
     in ((prId, Nothing) : arguments, newNewContext)
  Case (Var i) _ _ alts ->
    let binder = showPpr (flags context) i
        index = getIndexFromAlts context alts
     in ((binder, index) : arguments, context)
  _ -> (arguments, context)

-- _ -> error ("translateArgument: unsupported expression\n" ++ prettyCoreExpr expr)

getIndexFromAlts :: TranslationContext -> [Alt CoreBndr] -> (Maybe Int)
getIndexFromAlts context alts = case alts of
  [] -> error ("getIndexFromAlts: empty AltCon list")
  (Alt _ binds (Var (i))) : [] -> elemIndex (i) (binds)
  _ -> error ("getIndexFromAlts: more than one AltCon\n" ++ prettyCoreAltList (flags context) alts)

findRates :: String -> [(String, ([Int], [Int]))] -> Maybe ([Int], [Int])
findRates actorName list = lookup actorName list

-- Create signals based on process constructor input names and their rates
createSignals :: TranslationContext -> String -> [(String, Maybe Int)] -> TranslationContext
createSignals context pr inputs =
  let inputRates = case (findRates pr (actors context)) of
        Just (rates, _) -> rates
        Nothing -> error ("No rates found for actor: " ++ pr)
   in aux context pr inputs inputRates
  where
    aux currentContext currentPr currentInputs rates = case (currentInputs, rates) of
      ([], _) -> currentContext
      (_, []) -> currentContext
      ((inputHead, Just i) : inputTail, rateHead : rateTail) ->
        let (name, newContext) = (genSignalName currentContext)
            newSignal = IRSignal name (inputHead, i) (currentPr, rateHead)
            newNewContext = newContext {signals = (name, newSignal) : (signals newContext)}
         in aux newNewContext currentPr inputTail rateTail
      ((inputHead, Nothing) : inputTail, rateHead : rateTail) ->
        let (name, newContext) = (genSignalName currentContext)
            newSignal = IRSignal name (inputHead, getSourceRate newContext inputHead) (currentPr, rateHead)
            newNewContext = newContext {signals = (name, newSignal) : (signals newContext)}
         in aux newNewContext currentPr inputTail rateTail

genSignalName :: TranslationContext -> (String, TranslationContext)
genSignalName context =
  let counter = nameCounter context
      newName = "s_" ++ show counter
      newContext = context {nameCounter = counter + 1}
   in (newName, newContext)

getSourceRate :: TranslationContext -> String -> Int
getSourceRate context id =
  if elem id (systemInputs context)
    then 1
    else getRateFromConstructors id (map snd (constructors context))

getRateFromConstructors :: String -> [IRConstructor] -> Int
getRateFromConstructors id list = case list of
  [] -> -1
  prHead : prTail -> case prHead of
    IRDelay prId _ (_, _) ->
      if prId == id
        then 1
        else getRateFromConstructors id prTail
    IRActor prId _ _ (_, _) ->
      if prId == id
        then error ("TODO: implement getRateFromConstructors for actors " ++ prId)
        else getRateFromConstructors id prTail

-- system s_in = s_out
--   where
--     (s_out, s_1) = a_1 s_in (d_1 s_1)

-- translateOutput :: TranslationContext -> CoreExpr -> TranslationContext
-- translateOutput context e = case e of
--     Case e b _ alts ->
--     _ -> context

--     newSignal IRSignal "s_out" ("a_1", 1) ("output", 1)

--     a_1 context to output with s_out

-- Case(Var(ds_d1bd): (wild_00, Signal d_a1aj) {
--     Alt(DataAlt((,)):
-- 	    {s_out_a1a1, s_1_X1, } Var(s_out_a1a1)),
--     })

translateCoreExpr :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
translateCoreExpr context binder expr = case expr of
  Lam _ (Lam _ (Lam _ (App (App (App (Var (i)) _) _) _))) -> case (showPpr (flags context) i) of
    "delaySDF" -> createDelaySDF context binder expr
    _ -> error "expecting delaySDF got something else"
  Lam _ (Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (Var (i)) _) _) _) _) _) _) _) _) _)))) -> case (showPpr (flags context) i) of
    "actor22SDF" -> createActorSDF context Actor22 binder expr
    _ -> error "expecting actor22SDF got something else"
  Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (Var (i)) _) _) _) _) _) _) _))) -> case (showPpr (flags context) i) of
    "actor12SDF" -> createActorSDF context Actor12 binder expr
    _ -> error "expecting actor12SDF got something else"
  _ -> createFunction context binder expr

-- delaySDF
createDelaySDF :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
createDelaySDF context binder expr =
  let tokens = (getLits expr [])
      delayId = showPpr (flags context) binder
      newDelay = IRDelay delayId tokens ("", "")
      newActorsList = (delayId, ([1], [1])) : (actors context)
   in context {constructors = (delayId, newDelay) : (constructors context), actors = newActorsList}

getActorSplit :: ActorType -> Int
getActorSplit actorType = case actorType of
  Actor12 -> 1
  Actor22 -> 2
  _ -> error "getActorSplit: unsupported actor type"

-- actorSDF
createActorSDF :: TranslationContext -> ActorType -> CoreBndr -> CoreExpr -> TranslationContext
createActorSDF context actorType binder expr =
  let lits = getLits expr []
      maybeFunctionName = getFunctionName context expr
   in case maybeFunctionName of
        Nothing -> error "No function found for actor"
        Just functionName ->
          let (inRates, outRates) = splitAt (getActorSplit actorType) lits
              actorId = showPpr (flags context) binder
              newActor = IRActor actorId actorType functionName ([], [])
              newActorsList = (actorId, (inRates, outRates)) : (actors context)
           in context {actors = newActorsList, constructors = (actorId, newActor) : (constructors context)}

createFunction :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
createFunction context binder expr =
  let functionId = showPpr (flags context) binder
      newFunction = IRFunction functionId (Just expr)
   in context {functions = (functionId, newFunction) : (functions context)}

getFunctionName :: TranslationContext -> CoreExpr -> Maybe String
getFunctionName context expr = case expr of
  App e a ->
    let getFirst = getFunctionName context a
     in case getFirst of
          Just name -> Just name
          Nothing -> getFunctionName context e
  Lam _ e -> getFunctionName context e
  Let _ e -> getFunctionName context e
  Var v ->
    let name = showPpr (flags context) v
     in if any (\x -> case x of IRFunction functionName _ -> functionName == name) (map snd (functions context))
          then
            Just name
          else Nothing
  _ -> Nothing

getLits :: CoreExpr -> [Int] -> [Int]
getLits expr acc = case expr of
  Lit l -> (fromIntegral (litValue l)) : acc
  App e a -> getLits e (getLits a acc)
  Lam _ e -> getLits e acc
  Let _ e -> getLits e acc
  _ -> acc
