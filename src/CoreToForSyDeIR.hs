module CoreToForSyDeIR where

import CoreIR
import Data.List (elemIndex)
import ForSyDeIR
import GHC hiding (targetId)
import GHC.Core
import GHC.Driver.Ppr
import GHC.Types.Literal
import Prelude hiding (id)

-- | The `TranslationContext` is a data type which is used to pass around
-- context required to complete the translation of Core IR to ForSyDe IR.
-- The `TranslationContext` is also used to store constructors, signals,
-- and functions whilst they are being built during the translation.
data TranslationContext = TranslationContext
  { flags :: DynFlags, -- Stores `DynFlags` for safely obtaining strings
    constructors :: [(String, IRConstructor)], -- Associated list of pcIds and IRConstructors
    signals :: [(String, IRSignal)], -- Associated list of signalIds and IRSignals
    functions :: [(String, IRFunction)], -- Associated list of functionIds and IRFunctions
    systemInputs :: [String], -- List of global inputs to net list
    systemOutputs :: [String], -- List of global outputs to net list
    nameCounter :: Int, -- Counter used for naming signals
    pcRates :: [(String, ([Int], [Int]))], -- Associated list of pcIds and input and ouput rates
    bindings :: [(String, String)] -- Associated list of binderIds and pcIds
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
      nameCounter = 1,
      pcRates = [],
      bindings = []
    }

-- | Translates a `CoreProgram` into an `IRSystem` requires `DynFlags` to
-- safely convert GHC Core elements into strings. Starts within an empty
-- `TranslationContext`.
translateCoreProgram :: DynFlags -> CoreProgram -> IRSystem
translateCoreProgram dflags program =
  let finalContext = finaliseConstructors (foldl translateCoreBind (initialTranslationContext dflags) program)
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
      IRDelay pcId tokens (inputSignal, outputSignal) ->
        if pcId == sourceId
          then (constructorId, IRDelay pcId tokens (inputSignal, signalId))
          else
            if pcId == targetId
              then (constructorId, IRDelay pcId tokens (signalId, outputSignal))
              else (constructorId, constructor)
      IRActor pcId actorType functionId (inputSignals, outputSignals) ->
        if pcId == sourceId
          then (constructorId, IRActor pcId actorType functionId (inputSignals, signalId : outputSignals))
          else
            if pcId == targetId
              then (constructorId, IRActor pcId actorType functionId (signalId : inputSignals, outputSignals))
              else (constructorId, constructor)

translateCoreBind :: TranslationContext -> CoreBind -> TranslationContext
translateCoreBind context (NonRec b e) = case (showPpr (flags context) b) of
  "$trModule" -> context
  "system" -> translateSystem context e
  _ -> translateCoreExpr context b e
-- NOTE: Currently no SDF examples have a `Rec` in the top level of their Core
-- output, thus unsure what the expected outcome should be.
translateCoreBind _ (Rec _) = error "translateCoreBind: `Rec` used in top level of Core output"

translateSystem :: TranslationContext -> CoreExpr -> TranslationContext
translateSystem context expr = case expr of
  -- Skips the first two `Lam` as they relate to type information
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
        newNewContext = translateBinds newContext binds
     in updateSignals newNewContext
  Let (NonRec b e) out ->
    let newContext = translateOutputs context out
        binderId = showPpr (flags newContext) b
        (pcId, newNewContext) = translateBodyExpr newContext [] e
        newNewNewContext = newNewContext {bindings = (binderId, pcId) : (bindings newNewContext)}
     in updateSignals newNewNewContext
  _ -> error ("translateInputs: unsupported expression\n" ++ prettyCoreExpr (flags context) expr)

-- | Updates all the signals within `TranslationContext` which are temporarily
-- using a binderId as either a sourceid or targetId. Replaces them with their
-- associated pcId based on the bindings accumulated within the
-- `TranslationContext`.
updateSignals :: TranslationContext -> TranslationContext
updateSignals context =
  let newSignals = map (updateSignal) (signals context)
   in context {signals = newSignals}
  where
    updateSignal :: (String, IRSignal) -> (String, IRSignal)
    updateSignal (currentSignalId, currentSignal) = case currentSignal of
      IRSignal signalId (sourceId, sourceRate) (targetId, targetRate) ->
        case getIdFromBinder sourceId (bindings context) of
          Nothing -> case getIdFromBinder targetId (bindings context) of
            Nothing -> (currentSignalId, currentSignal)
            Just pcId ->
              let inputRates = case (lookup pcId (pcRates context)) of
                    Just (rates, _) -> rates
                    Nothing -> error ("No rates found for actor: " ++ pcId)
                  newSignal = IRSignal signalId (sourceId, sourceRate) (pcId, inputRates !! targetRate)
               in (currentSignalId, newSignal)
          Just pcId ->
            let outputRates = case (lookup pcId (pcRates context)) of
                  Just (_, rates) -> rates
                  Nothing -> error ("No rates found for actor: " ++ pcId)
                newSignal = IRSignal signalId (pcId, outputRates !! sourceRate) (targetId, targetRate)
             in (currentSignalId, newSignal)

-- | Helper function for `updateSignals` which returns pcId for inputed binderId
getIdFromBinder :: String -> [(String, String)] -> Maybe String
getIdFromBinder targetBinder binds = case binds of
  [] -> Nothing
  (binder, pcId) : bindTail ->
    if targetBinder == binder
      then Just pcId
      else getIdFromBinder targetBinder bindTail

translateOutputs :: TranslationContext -> CoreExpr -> TranslationContext
translateOutputs context expr = case expr of
  App e a -> translateOutputs (translateOutputs context a) e
  Var _ -> context
  Type _ -> context
  Case e _ _ alts -> case getVarFromAlts context alts of
    (_, Nothing) -> context
    (binder, Just index) ->
      let bind = showPpr (flags context) e
          (name, newContext) = genSignalName context
          newSignal = IRSignal name (bind, index) (binder, 1)
       in newContext {systemOutputs = binder : (systemOutputs newContext), signals = (name, newSignal) : (signals newContext)}
  Let (NonRec b e) a ->
    let newContext = translateOutputs context a
        binderId = showPpr (flags newContext) b
        (pcId, newNewContext) = translateBodyExpr newContext [] e
     in newNewContext {bindings = (binderId, pcId) : (bindings newNewContext)}
  _ -> error ("translateOutputs: unsupported expression\n" ++ prettyCoreExpr (flags context) expr)

-- | Helper function for `translateOutputs` which identifies variable chosen by
-- an `AltCon` of a `Case`. Returns the  identified variable as a string and its
-- index as an optional int.
getVarFromAlts :: TranslationContext -> [Alt CoreBndr] -> (String, Maybe Int)
getVarFromAlts context alts = case alts of
  [] -> error ("getIndexFromAlts: empty AltCon list")
  (Alt _ binds (Var (i))) : [] ->
    let binder = showPpr (flags context) i
        index = elemIndex (i) (binds)
     in (binder, index)
  _ -> error ("getIndexFromAlts: more than one AltCon\n" ++ prettyCoreAltList (flags context) alts)

translateBinds :: TranslationContext -> [(CoreBndr, CoreExpr)] -> (TranslationContext)
translateBinds context binds = case binds of
  [] -> (context)
  (binder, expr) : bindTail ->
    let binderId = showPpr (flags context) binder
        (pcId, newContext) = translateBodyExpr context [] expr
        newNewContext = newContext {bindings = (binderId, pcId) : (bindings newContext)}
     in translateBinds newNewContext bindTail

translateBodyExpr :: TranslationContext -> [(String, Maybe Int)] -> CoreExpr -> (String, TranslationContext)
translateBodyExpr context arguments expr = case expr of
  App (App (Var i) _) _ ->
    let pcId = showPpr (flags context) i
        newContext = createSignals context pcId arguments
     in (pcId, newContext)
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
        (pcId, newNewContext) = translateBodyExpr newContext newArguments e
     in ((pcId, Nothing) : arguments, newNewContext)
  Case (Var i) _ _ alts ->
    let binder = showPpr (flags context) i
        index = getIndexFromAlts context alts
     in ((binder, index) : arguments, context)
  _ -> error ("translateArgument: unsupported expression\n" ++ prettyCoreExpr (flags context) expr)

getIndexFromAlts :: TranslationContext -> [Alt CoreBndr] -> (Maybe Int)
getIndexFromAlts context alts = case alts of
  [] -> error ("getIndexFromAlts: empty AltCon list")
  (Alt _ binds (Var (i))) : [] -> elemIndex (i) (binds)
  _ -> error ("getIndexFromAlts: more than one AltCon\n" ++ prettyCoreAltList (flags context) alts)

-- | Creates signals based on the arguments of a process constructor. All
-- signals created with this function have the process constructor as the
-- target.
createSignals :: TranslationContext -> String -> [(String, Maybe Int)] -> TranslationContext
createSignals context pr arguments =
  -- The input rates of the process constructor are used to determine the
  -- targetRate of created signals, the index for the input is the same as the
  -- current arguments index.
  let inputRates = case (lookup pr (pcRates context)) of
        Just (rates, _) -> rates
        Nothing -> error ("No rates found for actor: " ++ pr)
   in aux context pr arguments inputRates
  where
    aux :: TranslationContext -> String -> [(String, Maybe Int)] -> [Int] -> TranslationContext
    aux currentContext currentPr currentArguments rates = case (currentArguments, rates) of
      ([], _) -> currentContext
      (_, []) -> currentContext
      ((inputHead, Just i) : inputTail, rateHead : rateTail) ->
        -- The optional int indicates the inputHead represents a binder rather
        -- than a process constructor. The optional int is the index for the
        -- output. Both binder and index will be replaced using the
        -- `updateSignals` function.
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

-- | Helper function for `createSignals` which gets the output rate for the
-- source of a signal. If the id is associated with a global input or delay
-- then the returned output rate is 1.
getSourceRate :: TranslationContext -> String -> Int
getSourceRate context id =
  if elem id (systemInputs context)
    then 1
    else aux (map snd (constructors context))
  where
    aux :: [IRConstructor] -> Int
    aux list = case list of
      [] -> error ("getSourceRate: " ++ id ++ " not in a valid constructor")
      pcHead : pcTail -> case pcHead of
        IRDelay pcId _ (_, _) ->
          if pcId == id
            then 1
            else aux pcTail
        IRActor pcId _ _ (_, _) ->
          -- If the id is associated with a actor a value is only returned if
          -- the identified actor has one output, otherwise the function
          -- returns an error.
          -- NOTE: This might be fine since if an actor has multiple outputs it
          -- would have been represented by binder with an optional int. Will
          -- require confirmation by exploring complex net lists.
          if pcId == id
            then
              let outputRates = case (lookup pcId (pcRates context)) of
                    Just (_, rates) -> rates
                    Nothing -> error ("getSourceRate: No rates found for actor " ++ pcId)
               in case outputRates of
                    [] -> error ("getSourceRate: Empty output rates for actor " ++ pcId)
                    [i] -> i
                    _ -> error ("getSourceRate: More than one output rate for actor " ++ pcId)
            else aux pcTail

-- | Creates actors and delays by pattern matching based on the number of
-- inputs to the top function, represented by `Lam`, and inputs to the process
-- constructor, represented by `App`. If it cannot match an actor or delay then
-- it creates a function.
translateCoreExpr :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
translateCoreExpr context binder expr = case expr of
  Lam _ (Lam _ (Lam _ (App (App (App (Var (i)) _) _) _))) ->
    let name = showPpr (flags context) i
     in case name of
          "delaySDF" -> createDelaySDF context binder expr
          _ -> error ("translateCoreExpr: expecting delaySDF got " ++ name)
  Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (Var (i)) _) _) _) _) _) _))) ->
    let name = showPpr (flags context) i
     in case name of
          "actor11SDF" -> createActorSDF context Actor11 binder expr
          _ -> error ("translateCoreExpr: expecting actor11SDF got " ++ name)
  Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (Var (i)) _) _) _) _) _) _) _))) ->
    let name = showPpr (flags context) i
     in case name of
          "actor12SDF" -> createActorSDF context Actor12 binder expr
          _ -> error ("translateCoreExpr: expecting actor12SDF got " ++ name)
  Lam _ (Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (App (Var (i)) _) _) _) _) _) _) _) _)))) ->
    let name = showPpr (flags context) i
     in case name of
          "actor21SDF" -> createActorSDF context Actor21 binder expr
          _ -> error ("translateCoreExpr: expecting actor21SDF got " ++ name)
  Lam _ (Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (Var (i)) _) _) _) _) _) _) _) _) _)))) ->
    let name = showPpr (flags context) i
     in case name of
          "actor22SDF" -> createActorSDF context Actor22 binder expr
          _ -> error ("translateCoreExpr: expecting actor22SDF got " ++ name)
  _ -> createFunction context binder expr

createDelaySDF :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
createDelaySDF context binder expr =
  let tokens = (getLits expr [])
      delayId = showPpr (flags context) binder
      newDelay = IRDelay delayId tokens ("", "")
      newActorsList = (delayId, ([1], [1])) : (pcRates context)
   in context {constructors = (delayId, newDelay) : (constructors context), pcRates = newActorsList}

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
              newActorsList = (actorId, (inRates, outRates)) : (pcRates context)
           in context {pcRates = newActorsList, constructors = (actorId, newActor) : (constructors context)}

-- | Helper function for `createDelaySDF` and `createActorSDF` which returns
-- all integer literals within their expression as a list.
getLits :: CoreExpr -> [Int] -> [Int]
getLits expr acc = case expr of
  Lit l -> (fromIntegral (litValue l)) : acc
  App e a -> getLits e (getLits a acc)
  Lam _ e -> getLits e acc
  Let _ e -> getLits e acc
  _ -> acc

-- | Helper function for `createActorSDF` which returns the index to split the
-- literals within an actor expression based on the actor type.
getActorSplit :: ActorType -> Int
getActorSplit actorType = case actorType of
  Actor11 -> 1
  Actor12 -> 1
  Actor21 -> 2
  Actor22 -> 2
  _ -> error "getActorSplit: unsupported actor type"

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
