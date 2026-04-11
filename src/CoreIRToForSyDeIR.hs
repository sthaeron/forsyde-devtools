module CoreIRToForSyDeIR (translateCoreProgram) where

import CoreIR
import Data.List (elemIndex)
import ForSyDeIR
import GHC hiding (targetId)
import GHC.Core
import GHC.Plugins

-- | The `TranslationContext` is a data type which is used to pass around
-- context required to complete the translation of Core IR to ForSyDe IR.
-- The `TranslationContext` is also used to store constructors, signals,
-- and functions whilst they are being built during the translation.
data TranslationContext = TranslationContext
  { flags :: DynFlags, -- Stores `DynFlags` for safely obtaining strings
    constructors :: [(IRId, IRConstructor)], -- Associated list of pcIds and IRConstructors
    signals :: [(IRId, IRSignal)], -- Associated list of signalIds and IRSignals
    functions :: [(IRId, IRFunction)], -- Associated list of functionIds and IRFunctions
    systemInputs :: [IRId], -- List of global inputs to netlist
    systemOutputs :: [IRId], -- List of global outputs to netlist
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
translateCoreProgram :: DynFlags -> CoreProgram -> Either String (IRSystem, [(IRId, IRSignal)])
translateCoreProgram dflags program = do
  finalContext <- foldl translateCoreBind (pure $ initialTranslationContext dflags) program
  pure
    ( IRSystem
        (systemInputs finalContext, systemOutputs finalContext)
        (map snd (constructors finalContext))
        (map snd (signals finalContext))
        (map snd (functions finalContext)),
      signals finalContext
    )

-- | Translates a top level `CoreBind`. Module information is currently ignored.
translateCoreBind :: Either String TranslationContext -> CoreBind -> Either String TranslationContext
translateCoreBind context (NonRec b e)
  | IRVar b == IRString "$trModule" = context
  | isSystemName (varName b) = context
  | IRVar b == IRString "system" = context >>= \c -> translateSystem c e
  | otherwise = context >>= \c -> translateCoreExpr c b e
-- NOTE: Currently no SDF examples have a `Rec` in the top level of their Core
-- output, thus unsure what the expected outcome should be.
translateCoreBind _ (Rec _) = Left "translateCoreBind - `Rec` used in top level of Core output"

-- | Translates the `CoreExpr` which is associated with the system netlist.
-- Identifies system inputs, builds the netlist through a `TranslationContext`,
-- and identifies system outputs.
translateSystem :: TranslationContext -> CoreExpr -> Either String TranslationContext
translateSystem initialContext expr = case expr of
  -- Explicitly ignores lambdas refering to type-level binders
  Lam b e | (isTyCoVar b || (isPredTy . varType) b) -> translateSystem initialContext e
  Lam b e ->
    let newInput = IRVar b
        context1 = initialContext {systemInputs = newInput : (systemInputs initialContext)}
     in translateSystem context1 e
  Let (Rec binds) out -> do
    context1 <- translateSystemBinds initialContext binds
    translateSystem context1 out
  Let (NonRec b e) out -> do
    -- A `NonRec` can just be translated as a single bind
    context1 <- translateSystemBinds initialContext [(b, e)]
    translateSystem context1 out
  Case (Let (Rec binds) out) _ _ _alts -> do
    context1 <- translateSystemBinds initialContext binds
    translateSystem context1 out
  _ -> do
    outputs <- translateOutputs initialContext expr
    context1 <- foldl' updateSystemOutput (pure initialContext) outputs
    let context2 =
          context1
            { systemInputs = reverse (systemInputs context1),
              systemOutputs = reverse (systemOutputs context1)
            }
    updateConstructorsAndSignals context2

-- | Helper function which identifies the outputs of a system `CoreExpr`.
translateOutputs :: TranslationContext -> CoreExpr -> Either String [IRId]
translateOutputs context expr = aux expr []
  where
    aux currentExpr outputs = case currentExpr of
      Var out -> pure $ (IRVar out) : outputs
      App (Var out) (Type _) -> pure $ (IRVar out) : outputs
      App _ (Type _) -> pure outputs
      App e (Var out) -> aux e ((IRVar out) : outputs)
      Lam _ e -> aux e outputs
      App e1 e2 -> aux e1 outputs >>= \acc -> aux e2 acc
      -- `Case` can be ignored, as the outputs defined within were translated
      -- when the binder was defined
      Case _ _ _ _alts -> pure outputs
      e -> Left $ "translateOutputs - Unsupported expression\n" ++ prettyCoreExpr (flags context) e

updateSystemOutput :: Either String TranslationContext -> IRId -> Either String TranslationContext
updateSystemOutput initialContext outputId = do
  context1 <- initialContext
  (sourceId, sourceRate) <- getSourceFromArgument context1 outputId
  if not (outputId `elem` (map fst (signals context1)))
    then
      let newSignal = IRSignal outputId (sourceId, sourceRate) (outputId, 1)
          context2 =
            context1
              { systemOutputs = outputId : (systemOutputs context1),
                signals = (outputId, newSignal) : (signals context1)
              }
       in pure context2
    else initialContext

-- | Updates all the signals within `TranslationContext` which are temporarily
-- using a binder as the signal source. Replaces them with their associated
-- process constructor based on the binders accumulated within the
-- `TranslationContext`. Also updates the outputs of the associated process
-- constructors. Since if its a source of to signal the signal is an output.
updateConstructorsAndSignals :: TranslationContext -> Either String TranslationContext
updateConstructorsAndSignals initialContext = do
  let initialSignals = map snd (signals initialContext)
  (context1, sigs) <- foldl' aux (pure (initialContext, [])) initialSignals
  pure context1 {signals = sigs}
  where
    aux :: Either String (TranslationContext, [(IRId, IRSignal)]) -> IRSignal -> Either String (TranslationContext, [(IRId, IRSignal)])
    aux acc' currentSignal@(IRSignal signalId (sourceId, sourceRate) (targetId, targetRate)) = do
      (currentContext, acc) <- acc'
      let maybebinder = lookup sourceId (binders currentContext)
       in case maybebinder of
            Just associatedbinder -> case associatedbinder of
              PcId pcId ->
                -- Signal source was a temporary binder, sourceRate represents
                -- the index of the output rather than a rate
                case (lookup pcId (pcRates currentContext)) of
                  Nothing -> Left ("updateConstructorsAndSignals - No rates found for process constructor: " ++ show pcId)
                  Just (_, outRates) -> do
                    let newSig = IRSignal signalId (pcId, outRates !! sourceRate) (targetId, targetRate)
                    context1 <- updateConstructorsOutputs currentContext signalId pcId sourceRate
                    pure (context1, (signalId, newSig) : acc)
              _ -> Left ("updateConstructorsAndSignals - Binder is not associated with any process constructors")
            Nothing -> do
              -- Signal source was already a process constructor meaning it
              -- only had 1 output, thus passing index zero
              context1 <- updateConstructorsOutputs currentContext signalId sourceId 0
              pure (context1, (signalId, currentSignal) : acc)

-- | Updates the outputs constructors within `TranslationContext`. It adds a
-- signal to the output list of a specific process constructor at the specified
-- index
updateConstructorsOutputs :: TranslationContext -> IRId -> IRId -> Int -> Either String TranslationContext
updateConstructorsOutputs initialContext signalId pcId index =
  case sequence $ map (updateConstructor) (constructors initialContext) of
    Left e -> Left e
    Right newConstructors -> pure initialContext {constructors = newConstructors}
  where
    updateConstructor :: (IRId, IRConstructor) -> Either String (IRId, IRConstructor)
    updateConstructor (currentPcId, currentConstructor) =
      if pcId == currentPcId
        then case currentConstructor of
          IRDelay _ tokens (inputSignal, _) ->
            if index /= 0
              then Left ("updateConstructorsOutputs - Process constructor id matches a delay but has a non-zero index")
              else
                let newConstructor = IRDelay currentPcId tokens (inputSignal, signalId)
                 in pure (currentPcId, newConstructor)
          IRActor _ actorType functionId (inputSignals, outputSignals) ->
            let newOutputSignals = take index outputSignals ++ [signalId] ++ drop (index + 1) outputSignals
                newConstructor = IRActor currentPcId actorType functionId (inputSignals, newOutputSignals)
             in pure (currentPcId, newConstructor)
        else pure (currentPcId, currentConstructor)

-- | Translates system binds and adds identified binders to `TranslationContext`
translateSystemBinds :: TranslationContext -> [(CoreBndr, CoreExpr)] -> Either String (TranslationContext)
translateSystemBinds initialcontext = foldl' translateSystemBind $ pure initialcontext
  where
    translateSystemBind initialContext (b, e) = do
      context <- initialContext
      (binder, context1) <- translateSystemExpr context e
      pure context1 {binders = (IRVar b, binder) : (binders context1)}

-- | Translates system `CoreExpr`. Identifies if the expression represents an
-- application of a process constructor or connection to a specific output of a
-- process constructor. In either case returns a `Binder` along side a
-- potentially updated `TranslationContext`.
translateSystemExpr :: TranslationContext -> CoreExpr -> Either String (Binder, TranslationContext)
translateSystemExpr initialContext expr = case expr of
  -- Explicitly ignores lambdas refering to type-level binders
  Lam b e | (isTyCoVar b || (isPredTy . varType) b) -> translateSystemExpr initialContext e
  Let (NonRec b e) out -> do
    let bindingId = IRVar b
    context1 <- translateSystemBinds initialContext [(b, e)]
    context2 <- translateSystem context1 out
    let binder = PcId bindingId
    pure (binder, context2)
  Case (Var i) _ _ alts -> do
    let bindingId = IRVar i
    index <- getIndexFromAlts initialContext alts
    let binder = Binding bindingId index
    pure (binder, initialContext)
  _ -> do
    (pcId, argumentIds) <- translateProcessApplication initialContext expr
    context1 <- createSignalsFromArguments initialContext pcId argumentIds
    let binder = PcId pcId
    pure (binder, context1)

-- | Helper function which identifies the process being applied and its arguments, within a system `CoreExpr`.
translateProcessApplication :: TranslationContext -> CoreExpr -> Either String (IRId, [IRId])
translateProcessApplication context expr = aux expr []
  where
    aux currentExpr arguments = case currentExpr of
      Var i -> pure (IRVar i, arguments)
      (App (App (Var i) (Type _)) (Var _)) -> pure (IRVar i, arguments)
      App e (Var a) | isId a -> aux e ((IRVar a) : arguments)
      App e (App (Var a) (Type _)) -> aux e ((IRVar a) : arguments)
      App e (Type _) -> aux e arguments
      Lam _ e -> aux e arguments
      e -> Left $ "translateProcessApplication - Unsupported CoreExpr:\n" ++ prettyCoreExpr (flags context) e

-- | Helper function which identifies the id chosen by an `AltCon` for a `Case`.
-- Returns the index of the identified id from a list of ids. These ids
-- represent the outputs of a process constructor.
getIndexFromAlts :: TranslationContext -> [Alt CoreBndr] -> Either String Int
getIndexFromAlts context alts = case alts of
  [] -> Left $ "getIndexFromAlts - Empty AltCon list"
  (Alt _ ids (Var (i))) : [] ->
    let maybeIndex = elemIndex i ids
     in case maybeIndex of
          Just index -> pure $ index
          Nothing -> Left $ "getIndexFromAlts - Unable to find: " ++ show (IRVar i)
  _ -> Left $ "getIndexFromAlts - More than one AltCon:\n" ++ prettyCoreAltList (flags context) alts

-- | Creates signals based on the arguments of a process constructor. All
-- signals created with this function have the process constructor as the
-- target.
createSignalsFromArguments :: TranslationContext -> IRId -> [IRId] -> Either String TranslationContext
createSignalsFromArguments context pcId arguments =
  -- The input rates of the process constructor are used to determine the
  -- targetRate of created signals, the index for the input is the same as the
  -- current arguments index.
  case (lookup pcId (pcRates context)) of
    Just (inputRates, _) -> foldr aux (pure context) $ zip arguments inputRates
    Nothing -> Left ("createSignalsFromArguments - No rates found for process constructor: " ++ show pcId)
  where
    aux :: (IRId, Int) -> Either String TranslationContext -> Either String TranslationContext
    aux (argument, rate) context0 = do
      currentContext <- context0
      case getSourceFromArgument currentContext argument of
        Left e -> Left e
        Right (sourceId, sourceRate) ->
          let newSignal = IRSignal argument (sourceId, sourceRate) (pcId, rate)
              newSignals = (argument, newSignal) : (signals currentContext)
              context1 = currentContext {signals = newSignals}
           in pure $ updateConstructorsInputs context1 pcId argument

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
getSourceFromArgument :: TranslationContext -> IRId -> Either String (IRId, Int)
getSourceFromArgument context argument =
  if elem argument (systemInputs context)
    then pure (argument, 1)
    else
      let maybeBinder = (lookup argument (binders context))
       in case maybeBinder of
            -- Argument is associated with a binder which directly represents a
            -- process constructor meaning said constructor only has 1 output.
            Just (PcId pcId) ->
              case (lookup pcId (pcRates context)) of
                Just (_, rates) -> pure (pcId, rates !! 0)
                Nothing -> Left ("getSourceFromArgument - No rates found for process constructor: " ++ show pcId)
            -- Argument is associated with a binder which indirectly represents
            -- a process constructor meaning said constructor has multiple
            -- outputs. Will temporarily use the binder as the signal source.
            -- This will be updated later in the translation when all binders
            -- have been identified.
            Just (Binding bindingId index) -> pure (bindingId, index)
            Nothing -> Left ("getSourceFromArgument - Unable to identify argument as a system input or binder: " ++ show argument)

-- Strip all Lams so we don't need to bother with non-eta-reduced processes.
-- We con't count the type variables as those won't produce an App in the top
-- level definition.
stripLams :: Integer -> CoreExpr -> (Integer, CoreExpr)
stripLams n expr = case expr of
  -- Explicitly strips lambdas refering to type-level binders
  Lam b e | (isTyCoVar b || (isPredTy . varType) b) -> stripLams n e
  Lam _ e | otherwise -> stripLams (n + 1) e
  _ -> (n, expr)

-- Strip n Apps if possible, otherwise Nothing
stripApps :: Integer -> CoreExpr -> Maybe CoreExpr
stripApps n expr = case expr of
  App e _ | n > 0 -> stripApps (n - 1) e
  _ | n > 0 -> Nothing
  _ | otherwise -> Just expr

-- | Creates actors and delays by pattern matching based on the number of
-- inputs to the top level function, represented by `Lam`, and inputs to the
-- process constructor, represented by `App`. If it cannot match an actor or
-- delay then it creates a function.
--
-- NOTE: This function needs to be updated with new pattern matches for
-- translation to support additional process constructors.
translateCoreExpr :: TranslationContext -> CoreBndr -> CoreExpr -> Either String TranslationContext
translateCoreExpr context' binder expr' =
  let (n, expr1) = stripLams 0 expr'
      (context, expr2) = case expr1 of
        Let (NonRec bn be) e -> (createFunction context' bn be, e)
        Let _ e -> (context', e)
        _ -> (context', expr1)
      expr = maybe expr' id (stripApps n expr2)
   in case expr of
        -- ForSyDe-Shallow SDF
        App (App (Var i) _) _
          | IRVar i == IRString "delaySDF" -> pure $ createDelaySDF context binder expr
        App (App (App (App (App (Var i) _) _) _) _) _
          | IRVar i == IRString "actor11SDF" -> createActorSDF context Actor11 binder expr
        App (App (App (App (App (App (Var i) _) _) _) _) _) _
          | IRVar i == IRString "actor12SDF" -> createActorSDF context Actor12 binder expr
          | IRVar i == IRString "actor21SDF" -> createActorSDF context Actor21 binder expr
        App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _
          | IRVar i == IRString "actor13SDF" -> createActorSDF context Actor13 binder expr
          | IRVar i == IRString "actor22SDF" -> createActorSDF context Actor22 binder expr
          | IRVar i == IRString "actor31SDF" -> createActorSDF context Actor31 binder expr
        App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _
          | IRVar i == IRString "actor14SDF" -> createActorSDF context Actor14 binder expr
          | IRVar i == IRString "actor23SDF" -> createActorSDF context Actor23 binder expr
          | IRVar i == IRString "actor32SDF" -> createActorSDF context Actor32 binder expr
          | IRVar i == IRString "actor41SDF" -> createActorSDF context Actor41 binder expr
        App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _
          | IRVar i == IRString "actor24SDF" -> createActorSDF context Actor24 binder expr
          | IRVar i == IRString "actor33SDF" -> createActorSDF context Actor33 binder expr
          | IRVar i == IRString "actor42SDF" -> createActorSDF context Actor42 binder expr
        App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _
          | IRVar i == IRString "actor34SDF" -> createActorSDF context Actor34 binder expr
          | IRVar i == IRString "actor43SDF" -> createActorSDF context Actor43 binder expr
        App (App (App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _) _) _
          | IRVar i == IRString "actor44SDF" -> createActorSDF context Actor44 binder expr
        -- ForSyDe-Atom SDF
        App (App (Var i) _) _
          | IRVar i == IRString "delay" -> pure $ createDelaySDF context binder expr
        App (App (App (Var i) _) _) _
          | IRVar i == IRString "actor11" -> createActorSDF context Actor11 binder expr
        App (App (App (App (Var i) _) _) _) _
          | IRVar i == IRString "actor12" -> createActorSDF context Actor12 binder expr
          | IRVar i == IRString "actor21" -> createActorSDF context Actor21 binder expr
        App (App (App (App (App (Var i) _) _) _) _) _
          | IRVar i == IRString "actor13" -> createActorSDF context Actor13 binder expr
          | IRVar i == IRString "actor22" -> createActorSDF context Actor22 binder expr
          | IRVar i == IRString "actor31" -> createActorSDF context Actor31 binder expr
        App (App (App (App (App (App (Var i) _) _) _) _) _) _
          | IRVar i == IRString "actor14" -> createActorSDF context Actor14 binder expr
          | IRVar i == IRString "actor23" -> createActorSDF context Actor23 binder expr
          | IRVar i == IRString "actor32" -> createActorSDF context Actor32 binder expr
          | IRVar i == IRString "actor41" -> createActorSDF context Actor41 binder expr
        App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _
          | IRVar i == IRString "actor24" -> createActorSDF context Actor24 binder expr
          | IRVar i == IRString "actor33" -> createActorSDF context Actor33 binder expr
          | IRVar i == IRString "actor42" -> createActorSDF context Actor42 binder expr
        App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _
          | IRVar i == IRString "actor34" -> createActorSDF context Actor34 binder expr
          | IRVar i == IRString "actor43" -> createActorSDF context Actor43 binder expr
        App (App (App (App (App (App (App (App (App (Var i) _) _) _) _) _) _) _) _) _
          | IRVar i == IRString "actor44" -> createActorSDF context Actor44 binder expr
        _ -> pure $ createFunction context binder expr

createDelaySDF :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
createDelaySDF context binder expr =
  let tokens = (getLits expr [])
      delayId = IRVar binder
      newDelay = IRDelay delayId tokens (Empty, Empty)
      newActorsList = (delayId, ([1], [1])) : (pcRates context)
   in context {constructors = (delayId, newDelay) : (constructors context), pcRates = newActorsList}

createActorSDF :: TranslationContext -> ActorType -> CoreBndr -> CoreExpr -> Either String TranslationContext
createActorSDF initialContext actorType binder expr =
  let lits = getLits expr []
      maybeFunctionName = getFunctionName initialContext expr
   in case maybeFunctionName of
        Nothing -> Left "createActorSDF - No function found for actor"
        Just functionName ->
          let (inputRates, outputRates) = splitAt (getActorSplit actorType) lits
              actorId = IRVar binder
              baseOutputs = replicate (length outputRates) Empty
              newActor = IRActor actorId actorType functionName ([], baseOutputs)
              newActors = (actorId, (inputRates, outputRates)) : (pcRates initialContext)
              newConstructors = (actorId, newActor) : (constructors initialContext)
              context1 = initialContext {pcRates = newActors, constructors = newConstructors}
           in pure context1

-- | Helper function for `createDelaySDF` and `createActorSDF` which returns
-- all integer literals within their expression as a list.
getLits :: CoreExpr -> [Int] -> [Int]
getLits expr acc = case expr of
  Lit l -> (literalToInt l) : acc
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
createFunction context binder expr = case expr of
  App (Var i) (Type _)
    | IRVar i == IRString "undefined" ->
        let functionId = IRVar binder
            newFunction = IRFunction functionId Nothing
         in context {functions = (functionId, newFunction) : (functions context)}
  _ ->
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
  -- Not the following results in undefined behaviour in C
  Var i | IRVar i == IRString "undefined" -> Just (IRString "NULL")
  Var i ->
    let name = IRVar i
     in if any (\x -> case x of IRFunction functionName _ -> functionName == name) (map snd (functions context))
          then
            Just name
          else Nothing
  _ -> Nothing
