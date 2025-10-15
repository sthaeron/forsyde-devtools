{-# LANGUAGE DeriveGeneric #-}

module SDF_schedule (computeScheduleAndBuffers, computeScheduleAndBuffersPrint) where

import Data.List (dropWhile, find, intercalate, nub)
import Data.Maybe (catMaybes)
import Data.Ratio (approxRational, denominator, numerator)
import ForSyDeIR
import GHC.Generics (Generic)
import Numeric.LinearAlgebra hiding (find)

-- | Convert ForSyDe IR to SDF data structures
convertIRSystem :: IRSystem -> ([Actor], [Edge])
convertIRSystem (IRSystem constructors signals _) =
  let -- 1. Delay node names (IRDelay has two parameters)
      delayNames = [n | IRDelay n _ <- constructors]

      -- 2. All actor names (excluding delays)
      allActorNames =
        [ n
          | IRActor n _ _ <- constructors
        ]

      -- 3. Actors that receive from "input"
      inputActorNames =
        [ dstId
          | IRSignal _ (srcId, _) (dstId, _) <- signals,
            srcId == "input"
        ]

      -- 4. Build actor list
      baseActors =
        [ Actor n (n `elem` inputActorNames)
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
            srcId /= "input",
            dstId /= "output",
            srcId `notElem` delayNames,
            dstId `notElem` delayNames
        ]

      -- 7. Folded delay edges (A -> delay -> B → becomes one edge)
      delayEdges = concatMap makeDelayEdge delayNames

      makeDelayEdge delayName =
        case ( Data.List.find (\(IRSignal _ (_, _) (d, _)) -> d == delayName) signals,
               Data.List.find (\(IRSignal _ (s, _) _) -> s == delayName) signals
             ) of
          ( Just (IRSignal _ (srcIn, prodIn) _),
            Just (IRSignal _ _ (dstOut, consOut))
            ) ->
              [ Edge
                  (findActorByName srcIn)
                  (findActorByName dstOut)
                  prodIn
                  consOut
                  True
                  1
              ]
          _ -> [] -- skip malformed delay

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
--  1. Choose a sign so the first non-zero component is positive.
--  2. Approximate each Double as a Rational within `epsilon`.
--  3. Scale by LCM of denominators to get integers.
--  4. Divide by GCD of non-zero entries to make the vector minimal.
--
-- Returns a list of Integers of same length as the input vector.
-- Edge cases:
--  - All-zero input -> returns all zeros.
--  - Components very close to 0 are treated as 0 by `epsilon` tolerance.
normalizeToInteger :: Vector R -> [Integer]
normalizeToInteger v =
  let epsilon :: Double
      epsilon = 1e-9 -- tolerance for approxRational
      components = toList v

      -- make the first non-zero entry positive (or leave zeros if all zero)
      signedComponents =
        case dropWhile (\x -> abs x < epsilon) components of
          [] -> components
          (x : _) -> if x < 0 then map negate components else components

      -- approximate each component as a Rational within epsilon
      rationals = map (`approxRational` epsilon) signedComponents

      -- scale up by lcm of denominators to get integer vector
      denominators = map denominator rationals
      lcmDenominator = if null denominators then 1 else foldl1 lcm denominators
      ints = [numerator r * (lcmDenominator `div` denominator r) | r <- rationals]

      -- divide by gcd of non-zero entries to make the vector minimal
      nonZeroInts = filter (/= 0) ints
      gcdAll = if null nonZeroInts then 1 else foldl1 gcd (map abs nonZeroInts)
   in map (`div` gcdAll) ints

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
    edgeLabel (Edge src dst _ _ _ _) = name src ++ "→" ++ name dst

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
                             = incomingEdgeIndices actors edges i
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
      mat = buildTopologyMatrixEdgesRows actors edges
      rankMat = rank mat
   in if rankMat == length actors - 1
        then
          let ns = computeNullSpace mat
              repVec = flatten (takeColumns 1 ns)
              repInt = normalizeToInteger repVec
              repCounts = map fromIntegral repInt :: [Int]
              schedIdxs = greedySchedule actors edges repCounts
              schedNames = map (name . (actors !!)) schedIdxs -- convert index to name
              bufSizes = simulateBufferUsage actors edges (map initTokens edges) schedIdxs
           in (schedNames, bufSizes)
        else error "Matrix rank is not equal to number of actors minus one. Cannot compute repetition vector."

----------------------------------------------------------
-- Pretty-print version
----------------------------------------------------------

-- | Prints topology matrix, repetition vector, schedule, verification results, and buffer usage.
computeScheduleAndBuffersPrint :: IRSystem -> IO ()
computeScheduleAndBuffersPrint irSystem = do
  let (actors, edges) = convertIRSystem irSystem
      mat = buildTopologyMatrixEdgesRows actors edges
  printMatrixEdgesRows edges actors mat

  let rankMat = rank mat
  putStrLn $ "\nMatrix Rank: " ++ show rankMat
  putStrLn $ "Number of actors: " ++ show (length actors)
  putStrLn $ "Number of edges: " ++ show (length edges)

  if rankMat == length actors - 1
    then do
      let ns = computeNullSpace mat
      putStrLn "\nNull Space (fractional repetition vector for ACTORS):"
      disp 4 ns

      let repVec = flatten (takeColumns 1 ns)
          repInt = normalizeToInteger repVec

      -- Verification: multiply the integer repetition vector back to the topology matrix
      let repVector = fromIntegral <$> repInt :: [R]
          verificationResult = mat #> vector repVector
          isZeroVector = all (\x -> abs x < 1e-9) (toList verificationResult)

      putStrLn "\nVerification of null space vector:"
      putStrLn $ "Repetition vector × Topology Matrix ≈ Zero Vector? " ++ show isZeroVector
      if not isZeroVector
        then do
          putStrLn "Warning: Product is not zero!"
          putStrLn "Product vector:"
          print verificationResult
        else return () -- verification passed
      putStrLn "\nNormalized repetition vector for ACTORS (integers):"
      let actorLabels = [name a | a <- actors]
      putStrLn $ intercalate " : " [label ++ "=" ++ show r | (label, r) <- zip actorLabels repInt]

      let repCounts = map fromIntegral repInt :: [Int]
          schedIdxs = greedySchedule actors edges repCounts
          schedNames = map (name . (actors !!)) schedIdxs

      putStrLn "\nGenerated schedule (actor firing order):"
      putStrLn $ intercalate ", " schedNames

      putStrLn "\nInitial tokens (provided by IR):"
      let edgeLabels = [name (src e) ++ "→" ++ name (dst e) | e <- edges]
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
-- Main
-- Run the whole process and print each step
----------------------------------------------------------

main :: IO ()
main = do
  let exampleSchedAndBufs = computeScheduleAndBuffers exampleSystem
      (schedIdxs, bufSizes) = exampleSchedAndBufs

  putStrLn "Schedule and buffer sizes (from library function):"
  putStrLn $ "Schedule indices: " ++ show schedIdxs
  putStrLn $ "Buffer sizes:     " ++ show bufSizes

  putStrLn "\n--- Pretty-print version ---\n"
  computeScheduleAndBuffersPrint exampleSystem
