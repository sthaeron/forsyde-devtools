{-# LANGUAGE DeriveGeneric #-}

module SDF_schedule (computeScheduleAndBuffers, computeScheduleAndBuffersPrint) where

import Data.List (dropWhile, find, intercalate, nub)
import Data.Maybe (catMaybes)
import Data.Ratio (approxRational, denominator, numerator)
import ForSyDeIR
import GHC
import GHC.Data.EnumSet as EnumSet
import GHC.Generics (Generic)
import GHC.Paths (libdir)
import GHC.Plugins
import GHC.Utils.Outputable
import Numeric.LinearAlgebra as LinearAlgebra hiding (find)

-- | Convert ForSyDe IR to SDF data structures
convertIRSystem :: IRSystem -> ([Actor], [Edge])
convertIRSystem (IRSystem (inputNames, outputNames) constructors signals _) =
  let -- 1. Delay node names (IRDelay has two parameters)
      delayNames = [n | IRDelay n _ (_, _) <- constructors]

      -- 2. All actor names (excluding delays)
      allActorNames =
        [ n
        | IRActor n _ _ (_, _) <- constructors
        ]

      -- 3. Actors that receive from "input"
      inputActorNames =
        [ dstId
        | IRSignal _ (srcId, _) (dstId, _) <- signals,
          srcId `elem` inputNames
        ]

      -- 4. Build actor list
      baseActors =
        [ Actor n (n `elem` inputActorNames || n `elem` inputNames)
        | n <- nub allActorNames
        ]

      -- 5. Find helper
      findActorByName n =
        case Data.List.find (\a -> name a == n) baseActors of
          Just a -> a
          Nothing -> error ("Actor not found: " ++ n)

      -- 6. Build normal edges (no delay, no input/output)
      normalEdges =
        [ Edge
            (findActorByName srcId)
            (findActorByName dstId)
            prodRate
            consRate
            False
            0
        | IRSignal _ (srcId, prodRate) (dstId, consRate) <- signals,
          srcId `notElem` inputNames,
          dstId `notElem` outputNames,
          srcId `notElem` delayNames,
          dstId `notElem` delayNames
        ]

      -- 7. Folded delay edges (A -> delay -> B → becomes one edge)
      delayEdges = concatMap makeDelayEdge delayNames

      makeDelayEdge delayName =
        let delayConstructor =
              case find
                ( \c -> case c of
                    IRDelay name _ (_, _) -> name == delayName
                    _ -> False
                )
                constructors of
                Just (IRDelay _ initTokens (_, _)) -> initTokens
                Nothing -> error $ "Delay node " ++ delayName ++ " not found in constructors"
            incoming =
              [ (srcId, prod)
              | IRSignal _ (srcId, prod) (dstId, _) <- signals,
                dstId == delayName
              ]
            outgoing =
              [ (dstId, cons)
              | IRSignal _ (srcId, _) (dstId, cons) <- signals,
                srcId == delayName
              ]
         in case (incoming, outgoing) of
              ([(srcIn, prodIn)], [(dstOut, consOut)]) ->
                [ Edge
                    (findActorByName srcIn)
                    (findActorByName dstOut)
                    prodIn
                    consOut
                    True
                    (length delayConstructor)
                ]
              ([], _) ->
                error $ "Delay node " ++ delayName ++ " has no input signal."
              (_, []) ->
                error $ "Delay node " ++ delayName ++ " has no output signal."
              _ ->
                error $ "Delay node " ++ delayName ++ " must have exactly one input and one output."

      -- 8. Normalize self-loops: ensure prod == cons, otherwise error
      normalizeSelfLoop e@(Edge src dst prod cons isDelay initTok)
        | name src == name dst =
            if prod /= cons
              then
                error $
                  "Invalid self-loop on actor "
                    ++ name src
                    ++ ": prod="
                    ++ show prod
                    ++ ", cons="
                    ++ show cons
              else e -- prod == cons is valid
        | otherwise = e

      finalEdges = map normalizeSelfLoop (normalEdges ++ delayEdges)
   in (baseActors, finalEdges)

----------------------------------------------------------
-- Data structures definitions
----------------------------------------------------------

data Actor = Actor
  { name :: String,
    isInput :: Bool
  }
  deriving (Show, Eq, Generic)

data Edge = Edge
  { src :: Actor,
    dst :: Actor,
    prod :: Int,
    cons :: Int,
    isDelay :: Bool,
    initTokens :: Int -- Count of Init tokens for delay edges
  }
  deriving (Show, Eq, Generic)

----------------------------------------------------------
-- Topology matrix and null space calculation
----------------------------------------------------------

-- | Build the topology matrix, where rows are edges and columns are actors
--
-- Topo building logic:
-- - If actor is the source of the edge, the value is the production rate
-- - If actor is the destination of the edge, the value is the negative consumption rate
-- - Otherwise, the value is 0
buildTopologyMatrixEdgesRows :: [Actor] -> [Edge] -> Matrix R
buildTopologyMatrixEdgesRows actors edges =
  fromLists [[topo actor edge | actor <- actors] | edge <- edges]
  where
    topo actor edge
      | src edge == dst edge = 0
      | src edge == actor = fromIntegral (prod edge)
      | dst edge == actor = -fromIntegral (cons edge)
      | otherwise = 0

-- | Compute the null space (repetition vector for actors)
computeNullSpace :: Matrix R -> Matrix R
computeNullSpace = nullspace

-- | Normalize the real null space vector to a minimal integer repetition vector
--
-- Steps:
--  1. Ignore near-zero components (|x| < 1e-12) when finding the smallest non-zero magnitude.
--  2. Divide all components by that smallest positive magnitude to obtain a proportional vector.
--  3. Multiply by a scaling factor (here 10000) and round to nearest integers for stability.
--  4. If all components are negative, flip the sign so the first non-zero becomes positive.
--  5. Divide by the GCD of all non-zero entries to produce the minimal integer vector.
--
-- Returns a list of Integers of same length as the input vector.
-- Edge cases:
--  - All-zero input -> returns all zeros.
--  - Components very close to 0 are treated as 0 by `epsilon` tolerance.
normalizeToInteger :: Vector Double -> [Integer]
normalizeToInteger v =
  let components = LinearAlgebra.toList v
      -- Find the smallest non-zero
      absComponents = map abs components
      nonZero = filter (\x -> x > 1e-12) absComponents
      minNonZero = if null nonZero then 1 else minimum (map abs nonZero)

      -- Round to integer
      scaled = map (\x -> round (x / minNonZero * 10000)) components

      -- Ensure positive
      finalScaled =
        if all (< 0) scaled
          then map abs scaled
          else scaled

      -- Normalize by GCD
      gcdAll = foldl1 gcd (map abs finalScaled)
   in if gcdAll == 0 then finalScaled else map (`div` gcdAll) finalScaled

-- | Print the matrix with edges as rows and actors as columns
printMatrixEdgesRows :: [Edge] -> [Actor] -> Matrix R -> IO ()
printMatrixEdgesRows edges actors matrix = do
  -- Calculate the maximum width of each column
  let actorNameWidths = map (length . name) actors
      rowValueWidths = map (maximum . map (length . show)) (toLists matrix)
      colWidths = zipWith max actorNameWidths rowValueWidths
      totalColWidths = map (+ 2) colWidths -- Add 2 to each column width for spacing

      -- Calculate the maximum width of the edge labels
      edgeLabelWidth = maximum (map (length . edgeLabel) edges)

  -- Print the header
  putStrLn "Topology Matrix (Edges × Actors):"
  putStrLn $
    pad (edgeLabelWidth + 2) "Edge\\Actor"
      ++ concatMap (\(actor, width) -> pad width (name actor)) (zip actors totalColWidths)

  -- Print the separator line
  let totalWidth = edgeLabelWidth + 2 + sum totalColWidths
  putStrLn $ replicate totalWidth '-'

  -- Print the matrix rows
  mapM_ (printRow edgeLabelWidth totalColWidths) (zip edges (toLists matrix))
  where
    pad width str = take width (str ++ repeat ' ')
    edgeLabel (Edge src dst _ _ _ _) = name src ++ " → " ++ name dst

    printRow labelWidth colWidths (edge, rowValues) =
      putStrLn $
        pad (labelWidth + 2) (edgeLabel edge)
          ++ concatMap
            (\(val, width) -> pad width (show val))
            (zip rowValues colWidths)

----------------------------------------------------------
-- Scheduler: Greedy + Forced approach with special handling for inputs.
-- Actors marked as isInput=True have a infinite external tokens
-- To fire an actor, it must have enough input tokens for all its incoming edges.
----------------------------------------------------------

-- | Get all the incoming edges for a given actor
-- Parameters:
-- - actors: list of actors
-- - edges: list of edges
-- - actorIndex: index of the actor
-- Returns: all edges satisfying: dst edge == actors[actorIndex]
incomingEdgeIndices :: [Actor] -> [Edge] -> Int -> [Int]
incomingEdgeIndices actors edges actorIndex =
  [idx | (idx, edge) <- zip [0 ..] edges, dst edge == (actors !! actorIndex)]

-- | Get all the outgoing edges for a given actor
-- Parameters:
-- The same as incomingEdgeIndices
-- Returns: all edges satisfying: src edge == actors[actorIndex]
outgoingEdgeIndices :: [Actor] -> [Edge] -> Int -> [Int]
outgoingEdgeIndices actors edges actorIndex =
  [idx | (idx, edge) <- zip [0 ..] edges, src edge == (actors !! actorIndex)]

-- | Update the element at a given index with a given function
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt index func list =
  let (before, element : after) = splitAt index list
   in before ++ (func element) : after

-- | Simulate one firing of a give actor
--
-- Parameters:
-- - actors: list of actors
-- - edges: list of edges
-- - actorIndex: index of the firing actor
-- - tokens: list of current amount of tokens for each edge
-- - minTokens: list of minimum tokens of each edge
-- Returns: the new tokens and the new minimum tokens
--
-- Steps:
-- 1. Get the incoming and outgoing edges for the firing actor
-- 2. Consume the tokens for the incoming edges with the consumption rate of the edge
-- 3. Produce the tokens for the outgoing edges with the production rate of the edge
-- 4. Update the minimum tokens
--
-- Returns: the new tokens and the new minimum tokens
fireOnce :: [Actor] -> [Edge] -> Int -> [Int] -> [Int]
fireOnce actors edges actorIndex tokens =
  let incomingEdges = incomingEdgeIndices actors edges actorIndex
      outgoingEdges = outgoingEdgeIndices actors edges actorIndex
      tokensAfterConsume = foldl consume tokens incomingEdges
      tokensAfterProduce = foldl produce tokensAfterConsume outgoingEdges
   in tokensAfterProduce
  where
    consume :: [Int] -> Int -> [Int]
    consume currentTokens edgeIndex =
      let consumption = cons (edges !! edgeIndex)
       in updateAt edgeIndex (subtract consumption) currentTokens
    produce :: [Int] -> Int -> [Int]
    produce currentTokens edgeIndex =
      let production = prod (edges !! edgeIndex)
       in updateAt edgeIndex (+ production) currentTokens

-- | Greedy forced schedule algorithm
--
-- Parameters:
--   actors: list of actors
--   edges: list of edges
--   repetitionCounts: number of times each actor needs to be executed
-- Return: schedule sequence
--
-- Steps:
-- 1. Find all the fireable actors (repetition count > 0 and has enough input tokens)
-- 2. Fire all the fireable actors with the sequence order (could be optimized for minimal buffer size)
-- 3. If there is no fireable actor, force fire an remaining actor with delay edges check (to determine the initial token count)
-- 4. Record the minimum token value of each edge during the execution
-- 5. If there are negative minimum token values, it will be the required initial token count
greedySchedule :: [Actor] -> [Edge] -> [Int] -> [Int]
greedySchedule actors edges repetitionCounts =
  let nActors = length actors
      initialTokens = map initTokens edges -- map initial tokens

      -- Recursive worker function:
      -- - remainingReps: remaining times
      -- - currentTokens: current token state
      -- - accSchedule: accumulated schedule sequence
      worker :: [Int] -> [Int] -> [Int] -> [Int]
      worker remainingReps currentTokens accSchedule
        | sum remainingReps == 0 = reverse accSchedule
        | otherwise =
            let isFireable i =
                  (remainingReps !! i) > 0
                    && let incomingEdges -- has remaining fire repetitions
                             =
                             incomingEdgeIndices actors edges i
                           actor = actors !! i
                           canFire = case incomingEdges of
                             [] ->
                               if isInput actor
                                 then True -- If is input actor and no incoming edges from other actors
                                 else error $ "Invalid graph: actor " ++ name actor ++ " has no incoming edges but is not marked as input."
                             _ ->
                               all
                                 ( \edgeIdx ->
                                     -- All incoming edges must have enough tokens
                                     currentTokens !! edgeIdx >= cons (edges !! edgeIdx)
                                 )
                                 incomingEdges
                        in canFire

                fireableActors = [i | i <- [0 .. nActors - 1], isFireable i]
             in case fireableActors of
                  (actorIdx : _) ->
                    -- If there is a fireable actor, fire the first one
                    let newTokens = fireOnce actors edges actorIdx currentTokens
                        newRemaining = updateAt actorIdx (subtract 1) remainingReps
                     in worker newRemaining newTokens (actorIdx : accSchedule)
                  [] -> error "Error: Deadlock detected, cannot find fireable actor.\n"
   in worker repetitionCounts initialTokens []

----------------------------------------------------------
-- Simulation to compute buffer size per edge
-- After generating a valid schedule, simulate it once over one period
-- The buffer size of each edge is defined as the maximum token count observed
----------------------------------------------------------

simulateBufferUsage :: [Actor] -> [Edge] -> [Int] -> [Int] -> [Int]
simulateBufferUsage actors edges initialTokens schedule =
  let nEdges = length edges
      -- Simulate one step：fire one actor, update token state and max buffer record
      step (currentTokens, maxBuffer) actorIndex =
        let incomingEdges = incomingEdgeIndices actors edges actorIndex
            outgoingEdges = outgoingEdgeIndices actors edges actorIndex
            tokensAfterConsume = foldl consume currentTokens incomingEdges
            tokensAfterProduce = foldl produce tokensAfterConsume outgoingEdges
            newMaxBuffer = zipWith max maxBuffer tokensAfterProduce
         in (tokensAfterProduce, newMaxBuffer)

      -- Use foldl to simulate the whole schedule
      (_, finalMaxBuffer) = foldl step (initialTokens, initialTokens) schedule
   in finalMaxBuffer
  where
    consume tokens edgeIdx =
      let consumption = cons (edges !! edgeIdx)
       in updateAt edgeIdx (subtract consumption) tokens
    produce tokens edgeIdx =
      let production = prod (edges !! edgeIdx)
       in updateAt edgeIdx (+ production) tokens

----------------------------------------------------------
-- Verification of the schedule
----------------------------------------------------------

verifySchedule :: [Actor] -> [Edge] -> [Int] -> [Int] -> [Int] -> Bool
verifySchedule actors edges initialTokens schedule _repetitionCounts =
  let -- Recursively check the schedule
      check :: [Int] -> [Int] -> Bool
      check currentTokens [] = True -- Base case: empty schedule is valid
      check currentTokens (actorIndex : remainingSchedule) =
        let incomingEdges = incomingEdgeIndices actors edges actorIndex

            -- Simulate consumption and check if it is valid
            tokensAfterConsume = foldl consume currentTokens incomingEdges
            consumptionValid = all (>= 0) tokensAfterConsume

            -- Simulate production
            outgoingEdges = outgoingEdgeIndices actors edges actorIndex
            tokensAfterProduce = foldl produce tokensAfterConsume outgoingEdges
         in consumptionValid && check tokensAfterProduce remainingSchedule
   in check initialTokens schedule
  where
    consume tokens edgeIdx =
      let consumption = cons (edges !! edgeIdx)
       in updateAt edgeIdx (subtract consumption) tokens
    produce tokens edgeIdx =
      let production = prod (edges !! edgeIdx)
       in updateAt edgeIdx (+ production) tokens

----------------------------------------------------------
-- Library Function: compute schedule & buffer sizes
----------------------------------------------------------

-- | Returns schedule as actor names and buffer sizes
computeScheduleAndBuffers :: IRSystem -> ([String], [Int])
computeScheduleAndBuffers irSystem =
  let (actors, edges) = convertIRSystem irSystem
   in if null edges
        then
          -- If there are no internal edges, fire all actor once, no buffer required
          let schedNames = map name actors
              bufSizes = replicate (length edges) 0
           in (schedNames, bufSizes)
        else
          let mat = buildTopologyMatrixEdgesRows actors edges
              rankMat = rank mat
           in if rankMat == length actors - 1
                then
                  let ns = computeNullSpace mat
                      repVec = flatten (takeColumns 1 ns)
                      repInt = normalizeToInteger repVec

                      -- Verification
                      repVector = fromIntegral <$> repInt :: [R]
                      verificationResult = mat #> vector repVector
                      isZeroVector = all (\x -> abs x < 1e-9) (LinearAlgebra.toList verificationResult)

                      -- 在这里计算最终结果，这样repInt在作用域内
                      finalResult =
                        if not isZeroVector
                          then error "Verification failed: repetition vector is not in null space"
                          else
                            let repCounts = map fromIntegral repInt :: [Int]
                                schedIdxs = greedySchedule actors edges repCounts
                                schedNames = map (name . (actors !!)) schedIdxs
                                bufSizes = simulateBufferUsage actors edges (map initTokens edges) schedIdxs
                             in (schedNames, bufSizes)
                   in finalResult
                else
                  error "Matrix rank is not equal to number of actors minus one. Cannot compute repetition vector."

----------------------------------------------------------
-- Pretty-print version
----------------------------------------------------------

-- | Prints topology matrix, repetition vector, schedule, verification results, and buffer usage.
computeScheduleAndBuffersPrint :: IRSystem -> IO ()
computeScheduleAndBuffersPrint irSystem = do
  let (actors, edges) = convertIRSystem irSystem

  if null edges
    then do
      -- If there are no internal edges, fire all actor once, no buffer required
      let schedNames = map name actors
          bufSizes = replicate (length edges) 0
      putStrLn "No internal edges found.\n"
      putStrLn "Schedule (all actors fire once):"
      putStrLn $ intercalate ", " schedNames
      putStrLn "\nBuffer sizes: no buffer needed."
    else do
      let mat = buildTopologyMatrixEdgesRows actors edges
      printMatrixEdgesRows edges actors mat

      let rankMat = rank mat
      putStrLn $ "\nMatrix Rank: " ++ show rankMat
      putStrLn $ "Number of actors: " ++ show (length actors)
      putStrLn $ "Number of edges: " ++ show (length edges)

      if rankMat == length actors - 1
        then do
          let ns = computeNullSpace mat
          putStrLn "\nNull Space (fractional repetition vector for actors):"
          disp 4 ns

          let repVec = flatten (takeColumns 1 ns)
              repInt = normalizeToInteger repVec

          -- Verification: multiply the integer repetition vector back to the topology matrix
          let repVector = fromIntegral <$> repInt :: [R]
              verificationResult = mat #> vector repVector
              isZeroVector = all (\x -> abs x < 1e-9) (LinearAlgebra.toList verificationResult)

          putStrLn "\nVerification of null space vector:"
          putStrLn $ "Topology Matrix × Repetition Vector ≈ Zero Vector? " ++ show isZeroVector
          if not isZeroVector
            then do
              putStrLn "Warning: Product is not zero!"
              putStrLn "Product vector:"
              print verificationResult
            else return () -- verification passed
          putStrLn "\nNormalized repetition vector for ACTORS (integers):"
          let actorLabels = [name a | a <- actors]
          putStrLn $ intercalate "\n" [label ++ "=" ++ show r | (label, r) <- zip actorLabels repInt]

          let repCounts = map fromIntegral repInt :: [Int]
              schedIdxs = greedySchedule actors edges repCounts
              schedNames = map (name . (actors !!)) schedIdxs

          putStrLn "\nGenerated schedule (actor firing order):"
          putStrLn $ intercalate ", " schedNames

          putStrLn "\nInitial tokens (provided by IR):"
          let edgeLabels = [name (src e) ++ " → " ++ name (dst e) | e <- edges]
          mapM_ (\(lbl, e) -> putStrLn $ " " ++ lbl ++ ": " ++ show (initTokens e)) (zip edgeLabels edges)

          let ok = verifySchedule actors edges (map initTokens edges) schedIdxs repCounts
          putStrLn $ "\nVerification of schedule with computed initial tokens: " ++ (if ok then "OK" else "FAILED")

          ------------------------------------------------------
          -- Simulate buffer usage for one period
          ------------------------------------------------------
          let bufSizes = simulateBufferUsage actors edges (map initTokens edges) schedIdxs
          putStrLn "\nSimulated buffer sizes (maximum tokens observed per edge):"
          mapM_ (\(lbl, sz) -> putStrLn $ "  " ++ lbl ++ ": " ++ show sz) (zip edgeLabels bufSizes)
        else
          putStrLn "\nMatrix rank is not equal to number of actors minus one. Cannot compute repetition vector."

----------------------------------------------------------
-- Example Systems to test the algorithm
----------------------------------------------------------
-- System with single actor and self loop
exampleSystem1 :: IRSystem
exampleSystem1 =
  IRSystem
    (["input"], ["output"])
    [ IRActor "actor_1" Actor22 "add" (["s_in", "s_2"], ["s_out", "s_1"]),
      IRDelay "delay_1" [0] ("s_1", "s_2")
    ]
    [ IRSignal "s_in" ("input", 1) ("actor_1", 1),
      IRSignal "s_1" ("actor_1", 1) ("delay_1", 1),
      IRSignal "s_2" ("delay_1", 1) ("actor_1", 1),
      IRSignal "s_out" ("actor_1", 1) ("output", 1)
    ]
    [ IRFunction "add" Nothing
    ]

-- System with single actor and nothing else
exampleSystem2 :: IRSystem
exampleSystem2 =
  IRSystem
    (["in"], ["out"])
    [ IRActor "actor" Actor11 "add" (["s_in"], ["s_out"])
    ]
    [ IRSignal "s_in" ("in", 1) ("actor", 1),
      IRSignal "s_out" ("actor", 1) ("out", 1)
    ]
    [ IRFunction "add" Nothing
    ]

-- System with two actors, one self loop
exampleSystem3 :: IRSystem
exampleSystem3 =
  IRSystem
    (["input"], ["output"])
    [ IRActor "actor_1" Actor22 "add" (["s_in", "s_2"], ["s_1", "s_3"]),
      IRDelay "delay_1" [0] ("s_1", "s_2"),
      IRActor "actor_2" Actor11 "add" (["s_3"], ["s_out"])
    ]
    [ IRSignal "s_in" ("input", 1) ("actor_1", 1),
      IRSignal "s_1" ("actor_1", 1) ("delay_1", 1),
      IRSignal "s_2" ("delay_1", 1) ("actor_1", 1),
      IRSignal "s_3" ("actor_1", 1) ("actor_2", 1),
      IRSignal "s_out" ("actor_2", 1) ("output", 1)
    ]
    [ IRFunction "add" Nothing
    ]

-- System with multiple inputs
exampleSystem4 :: IRSystem
exampleSystem4 =
  IRSystem
    (["s_ina", "s_inb"], ["s_out"])
    [ IRActor "actor_a" Actor11 "add" (["s_ina"], ["s_1"]),
      IRActor "actor_b" Actor11 "add" (["s_inb"], ["s_2"]),
      IRActor "actor_c" Actor21 "add" (["s_1", "s_4"], ["s_3"]),
      IRActor "actor_d" Actor22 "add" (["s_2", "s_3"], ["s_4_delay", "s_out"]),
      IRDelay "delay" [0] ("s_4_delay", "s_4")
    ]
    [ IRSignal "s_ina" ("s_ina", 1) ("actor_a", 2),
      IRSignal "s_inb" ("s_inb", 1) ("actor_b", 1),
      IRSignal "s_1" ("actor_a", 1) ("actor_c", 2),
      IRSignal "s_2" ("actor_b", 2) ("actor_d", 2),
      IRSignal "s_3" ("actor_c", 1) ("actor_d", 1),
      IRSignal "s_4_delay" ("actor_d", 1) ("delay", 1),
      IRSignal "s_4" ("delay", 1) ("actor_c", 1),
      IRSignal "s_out" ("actor_d", 2) ("s_out", 1)
    ]
    [ IRFunction "add" Nothing
    ]

exampleSystem5 :: IRSystem
exampleSystem5 =
  IRSystem
    (["s_in"], ["s_out"])
    [ IRActor "a" Actor21 "add" (["s_in", "s3"], ["s1"]),
      IRActor "b" Actor11 "add" (["s1"], ["s2"]),
      IRActor "c" Actor12 "add" (["s2"], ["s3_delay", "s_out"]),
      IRDelay "delay" [0, 0, 0, 0, 0, 0] ("s3_delay", "s3")
    ]
    [ IRSignal "s_in" ("s_in", 1) ("a", 2),
      IRSignal "s1" ("a", 1) ("b", 2),
      IRSignal "s2" ("b", 3) ("c", 1),
      IRSignal "s3_delay" ("c", 2) ("delay", 2),
      IRSignal "s3" ("delay", 3) ("a", 3),
      IRSignal "s_out" ("c", 1) ("s_out", 1)
    ]
    [ IRFunction "add" Nothing
    ]

exampleSystem6 :: IRSystem
exampleSystem6 =
  IRSystem
    (["s_in"], ["s_out"])
    [ IRActor "a" Actor12 "add" (["s_in", "s4"], ["s1"]),
      IRActor "b" Actor11 "add" (["s1"], ["s2_delay"]),
      IRActor "c" Actor11 "add" (["s3"], ["s4"]),
      IRActor "d" Actor12 "add" (["s2"], ["s3", "s_out"]),
      IRDelay "delay" [0, 0] ("s2_delay", "s2")
    ]
    [ IRSignal "s_in" ("s_in", 1) ("a", 2),
      IRSignal "s1" ("a", 1) ("b", 4),
      IRSignal "s2_delay" ("b", 1) ("delay", 1),
      IRSignal "s2" ("delay", 2) ("d", 2),
      IRSignal "s3" ("d", 4) ("c", 1),
      IRSignal "s4" ("c", 4) ("a", 2),
      IRSignal "s_out" ("d", 1) ("s_out", 1)
    ]
    [ IRFunction "add" Nothing
    ]

----------------------------------------------------------
-- Main
-- Run all the example Systems
----------------------------------------------------------
customDflags :: IO DynFlags
customDflags = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  return $
    updOptLevel 2 $
      dflags
        { ghcLink = NoLink,
          ghcMode = CompManager,
          verbosity = 0,
          debugLevel = 0,
          generalFlags =
            EnumSet.fromList
              [ Opt_SuppressTicks,
                Opt_SuppressCoercions,
                Opt_SuppressCoercionTypes,
                Opt_SuppressVarKinds,
                Opt_SuppressModulePrefixes,
                Opt_SuppressTypeApplications,
                Opt_SuppressIdInfo,
                Opt_SuppressUnfoldings,
                Opt_SuppressTypeSignatures,
                Opt_SuppressUniques,
                Opt_SuppressStgExts,
                Opt_SuppressStgReps,
                Opt_SuppressTimestamps,
                Opt_SuppressCoreSizes
              ]
        }

main :: IO ()
main = do
  let examples =
        [ ("Example System 1 (single actor with self-loop)", exampleSystem1),
          ("Example System 2 (single actor only)", exampleSystem2),
          ("Example System 3 (two actors, one self loop)", exampleSystem3),
          ("Example System 4 (System with multiple inputs)", exampleSystem4),
          ("Example System 5", exampleSystem5),
          ("Example System 6", exampleSystem6)
        ]

  mapM_ runExample examples
  where
    runExample (name, system) = do
      dflags <- customDflags
      putStrLn $ "========================================================="
      putStrLn $ "Running: " ++ name
      putStrLn $ "========================================================="
      putStrLn $ prettyIRSystem dflags system

      putStrLn "\n--- Detailed analysis ---\n"
      computeScheduleAndBuffersPrint system
      putStrLn "\n"
