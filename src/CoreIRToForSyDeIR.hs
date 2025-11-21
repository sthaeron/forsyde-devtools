module CoreIRToForSyDeIR where

import CoreIR
import Data.List (elemIndex)
import ForSyDeIR
import GHC hiding (targetId)
import GHC.Core
import GHC.Types.Literal

-- | The `TranslationContext` is a data type which is used to pass around
-- context required to complete the translation of Core IR to ForSyDe IR.
-- The `TranslationContext` is also used to store constructors, signals,
-- and functions whilst they are being built during the translation.
data TranslationContext = TranslationContext
  { flags :: DynFlags, -- Stores `DynFlags` for safely obtaining strings
    constructors :: [(IRId, IRConstructor)], -- Associated list of pcIds and IRConstructors
    signals :: [(IRId, IRSignal)], -- Associated list of signalIds and IRSignals
    functions :: [(IRId, IRFunction)], -- Associated list of functionIds and IRFunctions
    systemInputs :: [IRId], -- List of global inputs to net list
    systemOutputs :: [IRId], -- List of global outputs to net list
    nameCounter :: Int, -- Counter used for naming signals
    pcRates :: [(IRId, ([Int], [Int]))], -- Associated list of pcIds and input and ouput rates
    binders :: [(IRId, Binder)] -- Associated list of binderIds and Binders
  }

-- | `Binder` is a data type used to represent what a `CoreBndr` is associated
-- to. Can either directly represent a process constructor through `PcId` or
-- indirectly represent a process constructor through `Binding` with a
-- bindingId and index. Note that in the case of a `Binding` the bindingId is
-- associated with a multi-output process constructor and the index identifies
-- a specific output.
data Binder
  = PcId IRId
  | Binding IRId Int

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
      binders = []
    }

-- | Translates a `CoreProgram` into an `IRSystem` requires `DynFlags` to
-- safely convert GHC Core elements into strings. Starts with an empty
-- `TranslationContext`.
translateCoreProgram :: DynFlags -> CoreProgram -> (IRSystem, [(IRId, IRSignal)])
translateCoreProgram dflags program =
  let finalContext = foldl translateCoreBind (initialTranslationContext dflags) program
   in ( IRSystem
          (systemInputs finalContext, systemOutputs finalContext)
          (map snd (constructors finalContext))
          (map snd (signals finalContext))
          (map snd (functions finalContext)),
        signals finalContext
      )

-- | Translates a top level `CoreBind`. Module information is currently ignored.
translateCoreBind :: TranslationContext -> CoreBind -> TranslationContext
translateCoreBind context (NonRec b e) = case show (IRVar b) of
  "$trModule" -> context
  "system" -> translateSystem context e
  _ -> translateCoreExpr context b e
-- NOTE: Currently no SDF examples have a `Rec` in the top level of their Core
-- output, thus unsure what the expected outcome should be.
translateCoreBind _ (Rec _) = error "translateCoreBind - `Rec` used in top level of Core output"

-- | Translates the `CoreExpr` which is associated with the system net list.
-- Identifies system inputs, builds the net list through a `TranslationContext`,
-- and identifies system outputs.
translateSystem :: TranslationContext -> CoreExpr -> TranslationContext
translateSystem initialContext expr = case expr of
  Lam b e ->
    let newInput = IRVar b
        context1 = initialContext {systemInputs = newInput : (systemInputs initialContext)}
        context2 = translateSystem context1 e
     in context2
  Let (Rec binds) out ->
    let context1 = translateSystemBinds initialContext binds
        context2 = translateSystem context1 out
     in context2
  Let (NonRec b e) out ->
    -- A `NonRec` can just be translated as a single bind
    let context1 = translateSystemBinds initialContext [(b, e)]
        context2 = translateSystem context1 out
     in context2
  _ -> translateSystemOutputs initialContext expr

-- | Identifies the system outputs of the net list and does the final clean-up
-- of the `TranslationContext` by updating constructors and signals.
--
-- NOTE: This function needs to be updated with new pattern matches for
-- translation to support net lists with additional system ouputs.
translateSystemOutputs :: TranslationContext -> CoreExpr -> TranslationContext
translateSystemOutputs initialContext expr = case expr of
  -- System has 1 output
  Var out ->
    let context1 = updateSystemOutput initialContext out
        context2 = context1 {systemInputs = reverse (systemInputs context1)}
        context3 = updateConstructorsAndSignals context2
     in context3
  -- System has 2 outputs
  App (App (App (App (Var _) (Type _)) (Type _)) (Var out1)) (Var out2) ->
    let context1 = foldl updateSystemOutput initialContext [out1, out2]
        context2 =
          context1
            { systemInputs = reverse (systemInputs context1),
              systemOutputs = reverse (systemOutputs context1)
            }
        context3 = updateConstructorsAndSignals context2
     in context3
  -- System has 3 outputs
  App (App (App (App (App (App (Var _) (Type _)) (Type _)) (Type _)) (Var out1)) (Var out2)) (Var out3) ->
    let context1 = foldl updateSystemOutput initialContext [out1, out2, out3]
        context2 =
          context1
            { systemInputs = reverse (systemInputs context1),
              systemOutputs = reverse (systemOutputs context1)
            }
        context3 = updateConstructorsAndSignals context2
     in context3
  -- System has 4 outputs
  App (App (App (App (App (App (App (App (Var _) (Type _)) (Type _)) (Type _)) (Type _)) (Var out1)) (Var out2)) (Var out3)) (Var out4) ->
    let context1 = foldl updateSystemOutput initialContext [out1, out2, out3, out4]
        context2 =
          context1
            { systemInputs = reverse (systemInputs context1),
              systemOutputs = reverse (systemOutputs context1)
            }
        context3 = updateConstructorsAndSignals context2
     in context3
  _ -> error ("translateSystemOutputs - Unsupported expression\n" ++ prettyCoreExpr (flags initialContext) expr)

updateSystemOutput :: TranslationContext -> Id -> TranslationContext
updateSystemOutput initialContext output =
  let outputId = IRVar output
      (sourceId, sourceRate) = getSourceFromArgument initialContext outputId
      newSignal = IRSignal outputId (sourceId, sourceRate) (outputId, 1)
      context1 =
        initialContext
          { systemOutputs = outputId : (systemOutputs initialContext),
            signals = (outputId, newSignal) : (signals initialContext)
          }
   in context1

-- | Updates all the signals within `TranslationContext` which are temporarily
-- using a binder as the signal source. Replaces them with their associated
-- process constructor based on the binders accumulated within the
-- `TranslationContext`. Also updates the outputs of the associated process
-- constructors. Since if its a source of to signal the signal is an output.
updateConstructorsAndSignals :: TranslationContext -> TranslationContext
updateConstructorsAndSignals initialContext =
  let initialSignals = map snd (signals initialContext)
      context1 = aux initialContext initialSignals []
   in context1
  where
    aux :: TranslationContext -> [IRSignal] -> [(IRId, IRSignal)] -> TranslationContext
    aux currentContext currentSignals acc = case currentSignals of
      [] ->
        let context1 = currentContext {signals = acc}
         in context1
      currentSignal@(IRSignal signalId (sourceId, sourceRate) (targetId, targetRate)) : signalsTail ->
        let maybebinder = lookup sourceId (binders currentContext)
         in case maybebinder of
              Just associatedbinder -> case associatedbinder of
                PcId pcId ->
                  -- Signal source was a temporary binder, sourceRate represents
                  -- the index of the output rather than a rate
                  let outputRates = case (lookup pcId (pcRates currentContext)) of
                        Just (_, rates) -> rates
                        Nothing -> error ("updateConstructorsAndSignals - No rates found for process constructor: " ++ show pcId)
                      newSignal = IRSignal signalId (pcId, outputRates !! sourceRate) (targetId, targetRate)
                      context1 = updateConstructorsOutputs currentContext signalId pcId sourceRate
                   in aux context1 signalsTail ((signalId, newSignal) : acc)
                _ -> error ("updateConstructorsAndSignals - Binder is not associated with any process constructors")
              Nothing ->
                -- Signal source was already a process constructor meaning it
                -- only had 1 output, thus passing index zero
                let context1 = updateConstructorsOutputs currentContext signalId sourceId 0
                 in aux context1 signalsTail ((signalId, currentSignal) : acc)

-- | Updates the outputs constructors within `TranslationContext`. It adds a
-- signal to the output list of a specific process constructor at the specified
-- index
updateConstructorsOutputs :: TranslationContext -> IRId -> IRId -> Int -> TranslationContext
updateConstructorsOutputs initialContext signalId pcId index =
  let newConstructors = map (updateConstructor) (constructors initialContext)
      context1 = initialContext {constructors = newConstructors}
   in context1
  where
    updateConstructor :: (IRId, IRConstructor) -> (IRId, IRConstructor)
    updateConstructor (currentPcId, currentConstructor) =
      if pcId == currentPcId
        then case currentConstructor of
          IRDelay _ tokens (inputSignal, _) ->
            if index /= 0
              then error ("updateConstructorsOutputs - Process constructor id matches a delay but has a non-zero index")
              else
                let newConstructor = IRDelay currentPcId tokens (inputSignal, signalId)
                 in (currentPcId, newConstructor)
          IRActor _ actorType functionId (inputSignals, outputSignals) ->
            let newOutputSignals = take index outputSignals ++ [signalId] ++ drop (index + 1) outputSignals
                newConstructor = IRActor currentPcId actorType functionId (inputSignals, newOutputSignals)
             in (currentPcId, newConstructor)
        else (currentPcId, currentConstructor)

-- | Translates system binds and adds identified binders to `TranslationContext`
translateSystemBinds :: TranslationContext -> [(CoreBndr, CoreExpr)] -> (TranslationContext)
translateSystemBinds initialcontext = foldl' translateSystemBind initialcontext
  where
    translateSystemBind initialContext (b, e) =
      let (binder, context1) = translateSystemExpr initialContext e
       in context1 {binders = (IRVar b, binder) : (binders context1)}

-- | Translates system `CoreExpr`. Identifies if the expression represents an
-- application of a process constructor or connection to a specific output of a
-- process constructor. In either case returns a `Binder` along side a
-- potentially updated `TranslationContext`.
--
-- NOTE: This function needs to be updated with new pattern matches for
-- translation to support process constructors with more inputs.
translateSystemExpr :: TranslationContext -> CoreExpr -> (Binder, TranslationContext)
translateSystemExpr initialContext expr = case expr of
  -- Application of process constructors with 1 input
  App (Var i) (Var a) ->
    let pcId = IRVar i
        aId = IRVar a
        arguments = [aId]
        context1 = createSignalsFromArguments initialContext pcId arguments
        binder = PcId pcId
     in (binder, context1)
  -- Application of process constructors with 2 input
  App (App (Var i) (Var a1)) (Var a2) ->
    let pcId = IRVar i
        a1Id = IRVar a1
        a2Id = IRVar a2
        arguments = [a1Id, a2Id]
        context1 = createSignalsFromArguments initialContext pcId arguments
        binder = PcId pcId
     in (binder, context1)
  -- Application of process constructors with 3 input
  App (App (App (Var i) (Var a1)) (Var a2)) (Var a3) ->
    let pcId = IRVar i
        a1Id = IRVar a1
        a2Id = IRVar a2
        a3Id = IRVar a3
        arguments = [a1Id, a2Id, a3Id]
        context1 = createSignalsFromArguments initialContext pcId arguments
        binder = PcId pcId
     in (binder, context1)
  -- Application of process constructors with 4 input
  App (App (App (App (Var i) (Var a1)) (Var a2)) (Var a3)) (Var a4) ->
    let pcId = IRVar i
        a1Id = IRVar a1
        a2Id = IRVar a2
        a3Id = IRVar a3
        a4Id = IRVar a4
        arguments = [a1Id, a2Id, a3Id, a4Id]
        context1 = createSignalsFromArguments initialContext pcId arguments
        binder = PcId pcId
     in (binder, context1)
  Case (Var i) _ _ alts ->
    let bindingId = IRVar i
        index = getIndexFromAlts initialContext alts
        binder = Binding bindingId index
     in (binder, initialContext)
  _ -> error ("translateSystemExpr - Unsupported CoreExpr:\n" ++ prettyCoreExpr (flags initialContext) expr)

-- | Helper function which identifies the id chosen by an `AltCon` for a `Case`.
-- Returns the index of the identified id from a list of ids. These ids
-- represent the outputs of a process constructor.
getIndexFromAlts :: TranslationContext -> [Alt CoreBndr] -> Int
getIndexFromAlts context alts = case alts of
  [] -> error ("getIndexFromAlts - Empty AltCon list")
  (Alt _ ids (Var (i))) : [] ->
    let maybeIndex = elemIndex i ids
     in case maybeIndex of
          Just index -> index
          Nothing -> error ("getIndexFromAlts - Unable to find: " ++ show (IRVar i))
  _ -> error ("getIndexFromAlts - More than one AltCon:\n" ++ prettyCoreAltList (flags context) alts)

-- | Creates signals based on the arguments of a process constructor. All
-- signals created with this function have the process constructor as the
-- target.
createSignalsFromArguments :: TranslationContext -> IRId -> [IRId] -> TranslationContext
createSignalsFromArguments context pcId arguments =
  -- The input rates of the process constructor are used to determine the
  -- targetRate of created signals, the index for the input is the same as the
  -- current arguments index.
  let inputRates = case (lookup pcId (pcRates context)) of
        Just (rates, _) -> rates
        Nothing -> error ("createSignalsFromArguments - No rates found for process constructor: " ++ show pcId)
   in aux context (reverse arguments) (reverse inputRates)
  where
    aux :: TranslationContext -> [IRId] -> [Int] -> TranslationContext
    aux currentContext currentArguments rates = case (currentArguments, rates) of
      ([], _) -> currentContext
      (_, []) -> currentContext
      (argumentsHead : argumentsTail, ratesHead : ratesTail) ->
        let (sourceId, sourceRate) = getSourceFromArgument currentContext argumentsHead
            newSignal = IRSignal argumentsHead (sourceId, sourceRate) (pcId, ratesHead)
            newSignals = (argumentsHead, newSignal) : (signals currentContext)
            context1 = currentContext {signals = newSignals}
            context2 = updateConstructorsInputs context1 pcId argumentsHead
         in aux context2 argumentsTail ratesTail

-- | Updates the inputs of constructors within a `TranslationContext`. Adds a
-- signal to the head of a process constructors input signals list.
updateConstructorsInputs :: TranslationContext -> IRId -> IRId -> TranslationContext
updateConstructorsInputs initialContext pcId signalId =
  let newConstructors = map (updateConstructor) (constructors initialContext)
      context1 = initialContext {constructors = newConstructors}
   in context1
  where
    updateConstructor :: (IRId, IRConstructor) -> (IRId, IRConstructor)
    updateConstructor (currentPcId, currentConstructor) =
      if pcId == currentPcId
        then case currentConstructor of
          IRDelay _ tokens (_, outputSignal) ->
            let newConstructor = IRDelay currentPcId tokens (signalId, outputSignal)
             in (currentPcId, newConstructor)
          IRActor _ actorType functionId (inputSignals, outputSignals) ->
            let newConstructor = IRActor currentPcId actorType functionId (signalId : inputSignals, outputSignals)
             in (currentPcId, newConstructor)
        else (currentPcId, currentConstructor)

-- | Helper function for `createSignalsFromArguments` which returns the id and
-- rate for the source of a signal based on an argument.
--
-- NOTE: If argument does not directly represent a process constructor then a
-- temporary source is returned. It will have to be updated later by
-- `updateConstructorsAndSignals` when all binders have been identified.
getSourceFromArgument :: TranslationContext -> IRId -> (IRId, Int)
getSourceFromArgument context argument =
  if elem argument (systemInputs context)
    then (argument, 1)
    else
      let maybeBinder = (lookup argument (binders context))
       in case maybeBinder of
            -- Argument is associated with a binder which directly represents a
            -- process constructor meaning said constructor only has 1 output.
            Just (PcId pcId) ->
              let outputRates = case (lookup pcId (pcRates context)) of
                    Just (_, rates) -> rates
                    Nothing -> error ("getSourceFromArgument - No rates found for process constructor: " ++ show pcId)
               in (pcId, outputRates !! 0)
            -- Argument is associated with a binder which indirectly represents
            -- a process constructor meaning said constructor has multiple
            -- outputs. Will temporarily use the binder as the signal source.
            -- This will be updated later in the translation when all binders
            -- have been identified.
            Just (Binding bindingId index) -> (bindingId, index)
            Nothing -> error ("getSourceFromArgument - Unable to identify argument as a system input or binder: " ++ show argument)

-- | Creates actors and delays by pattern matching based on the number of
-- inputs to the top level function, represented by `Lam`, and inputs to the
-- process constructor, represented by `App`. If it cannot match an actor or
-- delay then it creates a function.
--
-- NOTE: This function needs to be updated with new pattern matches for
-- translation to support additional process constructors.
translateCoreExpr :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
translateCoreExpr context binder expr = case expr of
  Lam _ (App (App (App (Var i) _) _) _)
    | IRVar i == IRString "delaySDF" -> createDelaySDF context binder expr
  Lam _ (App (App (App (App (App (App (Var i) _) _) _) _) _) _)
    | IRVar i == IRString "actor11SDF" -> createActorSDF context Actor11 binder expr
  Lam _ (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _)
    | IRVar i == IRString "actor12SDF" -> createActorSDF context Actor12 binder expr
  Lam _ (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _)
    | IRVar i == IRString "actor13SDF" -> createActorSDF context Actor13 binder expr
  Lam _ (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _)
    | IRVar i == IRString "actor14SDF" -> createActorSDF context Actor14 binder expr
  Lam _ (Lam _ (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _))
    | IRVar i == IRString "actor21SDF" -> createActorSDF context Actor21 binder expr
  Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _))
    | IRVar i == IRString "actor22SDF" -> createActorSDF context Actor22 binder expr
  Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _))
    | IRVar i == IRString "actor23SDF" -> createActorSDF context Actor23 binder expr
  Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _) _))
    | IRVar i == IRString "actor24SDF" -> createActorSDF context Actor24 binder expr
  Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _)))
    | IRVar i == IRString "actor31SDF" -> createActorSDF context Actor31 binder expr
  Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _) _)))
    | IRVar i == IRString "actor32SDF" -> createActorSDF context Actor32 binder expr
  Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _) _) _)))
    | IRVar i == IRString "actor33SDF" -> createActorSDF context Actor33 binder expr
  Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _) _) _) _)))
    | IRVar i == IRString "actor34SDF" -> createActorSDF context Actor34 binder expr
  Lam _ (Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _) _) _))))
    | IRVar i == IRString "actor41SDF" -> createActorSDF context Actor41 binder expr
  Lam _ (Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _) _) _) _))))
    | IRVar i == IRString "actor42SDF" -> createActorSDF context Actor42 binder expr
  Lam _ (Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _) _) _) _) _))))
    | IRVar i == IRString "actor43SDF" -> createActorSDF context Actor43 binder expr
  Lam _ (Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _) _) _) _) _) _))))
    | IRVar i == IRString "actor44SDF" -> createActorSDF context Actor44 binder expr
  _ -> createFunction context binder expr

createDelaySDF :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
createDelaySDF context binder expr =
  let tokens = (getLits expr [])
      delayId = IRVar binder
      newDelay = IRDelay delayId tokens (Empty, Empty)
      newActorsList = (delayId, ([1], [1])) : (pcRates context)
   in context {constructors = (delayId, newDelay) : (constructors context), pcRates = newActorsList}

createActorSDF :: TranslationContext -> ActorType -> CoreBndr -> CoreExpr -> TranslationContext
createActorSDF initialContext actorType binder expr =
  let lits = getLits expr []
      maybeFunctionName = getFunctionName initialContext expr
   in case maybeFunctionName of
        Nothing -> error "createActorSDF - No function found for actor"
        Just functionName ->
          let (inputRates, outputRates) = splitAt (getActorSplit actorType) lits
              actorId = IRVar binder
              baseOutputs = replicate (length outputRates) Empty
              newActor = IRActor actorId actorType functionName ([], baseOutputs)
              newActors = (actorId, (inputRates, outputRates)) : (pcRates initialContext)
              newConstructors = (actorId, newActor) : (constructors initialContext)
              context1 = initialContext {pcRates = newActors, constructors = newConstructors}
           in context1

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
  Actor13 -> 1
  Actor14 -> 1
  Actor21 -> 2
  Actor22 -> 2
  Actor23 -> 2
  Actor24 -> 2
  Actor31 -> 3
  Actor32 -> 3
  Actor33 -> 3
  Actor34 -> 3
  Actor41 -> 4
  Actor42 -> 4
  Actor43 -> 4
  Actor44 -> 4

createFunction :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
createFunction context binder expr =
  let functionId = IRVar binder
      newFunction = IRFunction functionId (Just expr)
   in context {functions = (functionId, newFunction) : (functions context)}

-- | Helper function for `createActorSDF` which returns name of the function
-- used by the process constructor.
getFunctionName :: TranslationContext -> CoreExpr -> Maybe IRId
getFunctionName context expr = case expr of
  App e a ->
    let getFirst = getFunctionName context a
     in case getFirst of
          Just name -> Just name
          Nothing -> getFunctionName context e
  Lam _ e -> getFunctionName context e
  Let _ e -> getFunctionName context e
  Var v ->
    let name = IRVar v
     in if any (\x -> case x of IRFunction functionName _ -> functionName == name) (map snd (functions context))
          then
            Just name
          else Nothing
  _ -> Nothing
