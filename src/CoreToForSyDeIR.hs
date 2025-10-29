module CoreToForSyDeIR where

import CoreIR
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
  let finalState = foldl translateCoreBind (initialTranslationContext dflags) binds
   in IRSystem
        (systemInputs finalState, systemOutputs finalState)
        (map snd (constructors finalState))
        (map snd (signals finalState))
        (map snd (functions finalState))

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
    let (_, newContext) = translateBinds context binds
     in newContext
  Let (NonRec b e) out ->
    let (bind1, newContext) = translateOutputs context out
        bind2 = showPpr (flags context) b
        (_, newNewContext) = translateBodyExpr newContext [] e
     in newNewContext
  _ -> error ("TranslateInputs: unsupported expression\n" ++ prettyCoreExpr (flags context) expr)

translateOutputs :: TranslationContext -> CoreExpr -> (Maybe String, TranslationContext)
translateOutputs context expr = case expr of
  App e a ->
    let (_, newContext) = translateOutputs context a
     in translateOutputs newContext e
  Var _ -> (Nothing, context)
  Type _ -> (Nothing, context)
  Case e _ _ alts ->
    let bind = showPpr (flags context) e
     in (Just bind, translateAlts context alts)
  _ -> error ("translateOutputs: unsupported expression\n" ++ prettyCoreExpr (flags context) expr)

translateAlts :: TranslationContext -> [Alt CoreBndr] -> TranslationContext
translateAlts context alts = case alts of
  [] -> context
  (Alt _ _ (Var (i))) : altTail ->
    let newOutput = showPpr (flags context) i
        newContext = context {systemOutputs = newOutput : (systemOutputs context)}
     in translateAlts newContext altTail
  _ -> error ("translateAlts: AltCon is not supported\n" ++ prettyCoreAltList (flags context) alts)

-- App(App(App(App(Var((,)) * Type(Signal c_a1d4)) * Type(Signal c_a1d4)) *
--   Case(Var(ds_d1dV): (wild_00, Signal c_a1d4)
-- 	{Alt(DataAlt((,)):
-- 	{s_out1_a1cZ, s_out2_a1d0, }Var(s_out1_a1cZ)), })) *
-- 	Case(Var(ds_d1dV): (wild_00, Signal c_a1d4)
-- 	{Alt(DataAlt((,)):
-- 	{s_out1_a1cZ, s_out2_a1d0, }Var(s_out2_a1d0)), })))))))

translateBinds :: TranslationContext -> [(CoreBndr, CoreExpr)] -> ([(String, String)], TranslationContext)
translateBinds context binds = translateBindsAux context binds []

translateBindsAux :: TranslationContext -> [(CoreBndr, CoreExpr)] -> [(String, String)] -> ([(String, String)], TranslationContext)
translateBindsAux context binds acc = case binds of
  [] -> (acc, context)
  (binder, expr) : bindTail ->
    let outputId = showPpr (flags context) binder
        (prId, newContext) = translateBodyExpr context [] expr
     in translateBindsAux newContext bindTail ((outputId, prId) : acc)

translateBodyExpr :: TranslationContext -> [String] -> CoreExpr -> (String, TranslationContext)
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
translateArgument :: TranslationContext -> [String] -> CoreExpr -> ([String], TranslationContext)
translateArgument context arguments expr = case expr of
  Var i -> let id = showPpr (flags context) i in (id : arguments, context)
  App e a ->
    let (newArguments, newContext) = translateArgument context [] a
        (prId, newNewContext) = translateBodyExpr newContext newArguments e
     in (prId : arguments, newNewContext)
  -- Case (Var i) _ _ _ -> let id = showPpr (flags context) i in (id : arguments, context)
  _ -> (arguments, context)

-- _ -> error ("translateArgument: unsupported expression\n" ++ prettyCoreExpr expr)

findRates :: String -> [(String, ([Int], [Int]))] -> Maybe ([Int], [Int])
findRates actorName list = lookup actorName list

-- Create signals based on process constr input names and their rates
createSignals :: TranslationContext -> String -> [String] -> TranslationContext
createSignals context pr inputs =
  let inputRates = case (findRates pr (actors context)) of
        Just (rates, _) -> rates
        Nothing -> error ("No rates found for actor: " ++ pr)
   in createSignalsAux context pr inputs inputRates

createSignalsAux :: TranslationContext -> String -> [String] -> [Int] -> TranslationContext
createSignalsAux context pr inputs rates = case (inputs, rates) of
  ([], _) -> context
  (_, []) -> context
  (inputHead : inputTail, rateHead : rateTail) ->
    let (newName, newContext) = (genSignalName context)
        newSignal = IRSignal newName (inputHead, getSourceRate newContext inputHead) (pr, rateHead)
        newNewContext = newContext {signals = (newName, newSignal) : (signals newContext)}
     in createSignalsAux newNewContext pr inputTail rateTail

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
  [] -> error ("No constructor found for id: " ++ id)
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
   in context {constructors = (delayId, newDelay) : (constructors context)}

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
              newActor = IRActor actorId actorType functionName ([""], [""])
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
