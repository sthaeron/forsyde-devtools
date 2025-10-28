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
    constructors :: [IRConstructor],
    signals :: [IRSignal],
    functions :: [IRFunction],
    systemInputs :: [String],
    systemOutputs :: [String],
    variables :: [String],
    nameCounter :: Int,
    actors :: [(String, ([Int], [Int]))], -- [(actorId, (inputRates, outputRates))]
    io :: [(String, ([String], [String]))] -- [(constructorId, (inputSignals, outputSignals))]
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
      variables = [],
      nameCounter = 0,
      actors = [], -- (actorIds, (inputRates, outputRates))
      io = [] -- (constructorIds, (inputSignals, outputSignals))
    }

translateCoreProgram :: DynFlags -> [CoreBind] -> IRSystem
translateCoreProgram dflags binds =
  let finalState = foldl translateCoreBind (initialTranslationContext dflags) binds
   in IRSystem
        (systemInputs finalState, systemOutputs finalState)
        (constructors finalState)
        (signals finalState)
        (functions finalState)

translateCoreBind :: TranslationContext -> CoreBind -> TranslationContext
translateCoreBind context (NonRec b e) = case (showPpr (flags context) b) of
  "$trModule" -> context
  "system" -> translateSystem context e
  _ -> translateCoreExpr context b e
translateCoreBind context (Rec _) = context

-- translateCoreBind context (Rec binds) =

translateSystem :: TranslationContext -> CoreExpr -> TranslationContext
translateSystem context e = case e of
  Lam _ (Lam _ e) -> translateInputs context e
  _ -> context

translateInputs :: TranslationContext -> CoreExpr -> TranslationContext
translateInputs context e = case e of
  Lam b e ->
    let newInput = showPpr (flags context) b
        newContext = context {systemInputs = newInput : (systemInputs context)}
     in translateInputs newContext e
  Let (Rec binds) out ->
    let (_, newContext) = translateBinds context binds
     in newContext
  Let (NonRec b e) out ->
    let newContext = translateOutputs context out
        bind = showPpr (flags context) b
        (_, newNewContext) = translateBodyExpr newContext [] e
     in newNewContext
  _ -> error ("TranslateInputs: unsupported expression\n" ++ prettyCoreExpr (flags context) e)

translateOutputs :: TranslationContext -> CoreExpr -> TranslationContext
translateOutputs context e = case e of
  App e a -> translateOutputs (translateOutputs context a) e
  Var _ -> context
  Type _ -> context
  Case e _ _ alts ->
    let bind = showPpr (flags context) e
     in translateAlts context alts
  _ -> error ("translateOutputs: unsupported expression\n" ++ prettyCoreExpr (flags context) e)

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
  (b, e) : bindTail ->
    let outputId = showPpr (flags context) b
        (prId, newContext) = translateBodyExpr context [] e
     in translateBindsAux newContext bindTail ((outputId, prId) : acc)

translateBodyExpr :: TranslationContext -> [String] -> CoreExpr -> (String, TranslationContext)
translateBodyExpr context arguments e = case e of
  App (App (Var i) _) _ ->
    let prId = showPpr (flags context) i
        newContext = createSignals context prId arguments
     in (prId, newContext)
  App e a ->
    let (newArguments, newContext) = translateArgument context arguments a
     in translateBodyExpr newContext newArguments e
  _ -> error ("translateBodyExpr: unsupported expression\n" ++ prettyCoreExpr (flags context) e)

-- App(App(
-- 	App(App(Var(a_1) * Type(d_a1aj)) *  Var($dNum_a1aw))
-- 		 * Var(s_in_a183)) * App(App(App(Var(d_1) * Type(d_a1aj)) * Var($dNum_a1aw)) * Case(Var(ds_d1bd): (wild_00, Signal d_a1aj)

-- Let(NonRec(ds_d1fv:
-- 	App(App(App(App(Var(a_1) * Type(d_a1et)) *
-- 	Var(s_in1_a1cK)) *
-- 	Var(s_in2_a1cL)))

-- Returns a list of arguments
translateArgument :: TranslationContext -> [String] -> CoreExpr -> ([String], TranslationContext)
translateArgument context arguments e = case e of
  Var i -> let sId = showPpr (flags context) i in (sId : arguments, context)
  App e a ->
    let (newArguments, newContext) = translateArgument context [] a
        (prId, newNewContext) = translateBodyExpr newContext newArguments e
     in (prId : arguments, newNewContext)
  -- Case (Var i) _ _ _ -> let sId = showPpr (flags context) i in (sId : arguments, context)
  _ -> (arguments, context)

-- _ -> error ("translateArgument: unsupported expression\n" ++ prettyCoreExpr e)

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
        newNewContext = newContext {signals = newSignal : (signals newContext)}
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
    else getRateFromConstructors id (constructors context)

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

-- if prId == id then
--   let
--     outputRates = case (findRates prId (actors context)) of
--       Just (_, rates) -> rates
--       Nothing -> error "No rates found for actor: " ++ prId
--   in
-- else getRateFromConstructors id tail

--   Case e b _ alts ->
-- Let(Rec({(ds_d1bd, App(App(App(App(Var(a_1) * Type(d_a1aj)) *  Var($dNum_a1aw)) * Var(s_in_a183)) * App(App(App(Var(d_1) * Type(d_a1aj)) * Var($dNum_a1aw)) *

-- Case(Var(ds_d1bd): (wild_00, Signal d_a1aj)
--  {Alt(DataAlt((,)):
--  {s_out_a1a1, s_1_X1, }Var(s_1_X1)), })))), })
--   let
--     newVar = showPpr (flags context) b
--     newContext = context { variables = newVar : (variables context) }
--   in

-- system s_in = s_out
--   where
--     (s_out, s_1) = a_1 s_in (d_1 s_1)

-- translateOutput :: TranslationContext -> CoreExpr -> TranslationContext
-- translateOutput context e = case e of
--     Case e b _ alts ->
--     _ -> context

--     newSignal IRSignal "s_out" ("a_1", 1) ("output", 1)

--     a_1 context to output with s_out

-- -- Case(Var(ds_d1bd): (wild_00, Signal d_a1aj) {
-- --     Alt(DataAlt((,)):
-- -- 	    {s_out_a1a1, s_1_X1, } Var(s_out_a1a1)),
-- --     })

translateCoreExpr :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
translateCoreExpr context b e = case e of
  Lam _ (Lam _ (Lam _ (App (App (App (Var (i)) _) _) _))) -> case (showPpr (flags context) i) of
    "delaySDF" -> createDelaySDF context b e
    _ -> error "expecting delaySDF got something else"
  Lam _ (Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (App (App (Var (i)) _) _) _) _) _) _) _) _) _)))) -> case (showPpr (flags context) i) of
    "actor22SDF" -> createActorSDF context Actor22 b e
    _ -> error "expecting actor22SDF got something else"
  Lam _ (Lam _ (Lam _ (App (App (App (App (App (App (App (Var (i)) _) _) _) _) _) _) _))) -> case (showPpr (flags context) i) of
    "actor12SDF" -> createActorSDF context Actor12 b e
    _ -> error "expecting actor12SDF got something else"
  _ -> createFunction context b e

-- delaySDF
createDelaySDF :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
createDelaySDF context b e =
  let tokens = (getLits e [])
      delayId = showPpr (flags context) b
      newDelay = IRDelay delayId tokens ("", "")
   in context {constructors = newDelay : (constructors context)}

getActorSplit :: ActorType -> Int
getActorSplit ac = case ac of
  Actor12 -> 1
  Actor22 -> 2
  _ -> error "getActorSplit: unsupported actor type"

-- actorSDF
createActorSDF :: TranslationContext -> ActorType -> CoreBndr -> CoreExpr -> TranslationContext
createActorSDF context ac b e =
  let lits = getLits e []
      maybeFunctionName = getFunctionName context e
   in case maybeFunctionName of
        Nothing -> error "No function found for actor"
        Just functionName ->
          let (inRates, outRates) = splitAt (getActorSplit ac) lits
              actorId = showPpr (flags context) b
              newActor = IRActor actorId ac functionName ([""], [""])
              newActorsList = (actorId, (inRates, outRates)) : (actors context)
           in context {actors = newActorsList, constructors = newActor : (constructors context)}

createFunction :: TranslationContext -> CoreBndr -> CoreExpr -> TranslationContext
createFunction context b e =
  let functionName = showPpr (flags context) b
      newFunction = IRFunction functionName (Just e)
   in context {functions = newFunction : (functions context)}

getFunctionName :: TranslationContext -> CoreExpr -> Maybe String
getFunctionName context e = case e of
  App e a ->
    let getFirst = getFunctionName context a
     in case getFirst of
          Just name -> Just name
          Nothing -> getFunctionName context e
  Lam _ e -> getFunctionName context e
  Let _ e -> getFunctionName context e
  Var v ->
    let name = showPpr (flags context) v
     in if any (\x -> case x of IRFunction functionName _ -> functionName == name) (functions context)
          then
            Just name
          else Nothing
  _ -> Nothing

getLits :: CoreExpr -> [Int] -> [Int]
getLits e acc = case e of
  Lit l -> (fromIntegral (litValue l)) : acc
  App e a -> getLits e (getLits a acc)
  Lam _ e -> getLits e acc
  Let _ e -> getLits e acc
  _ -> acc
