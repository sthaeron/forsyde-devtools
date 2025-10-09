{-# LANGUAGE DeriveGeneric #-}
import Numeric.LinearAlgebra
import Data.List (intercalate, findIndex, dropWhile)
import GHC.Generics (Generic)
import Data.Ratio (approxRational, numerator, denominator)

----------------------------------------------------------
-- Data structures definitions
----------------------------------------------------------

data Actor = Actor { name :: String, isInput :: Bool, isOutput :: Bool } deriving (Show, Eq, Generic)
data Edge  = Edge  { src :: Actor, dst :: Actor, prod :: Int, cons :: Int } deriving (Show, Eq, Generic)

----------------------------------------------------------
-- Example graph
-- Incoming and outgoing edges are discarded
-- But scheduling will need to know the actor with input to initiate with
----------------------------------------------------------

a = Actor "A" True False
b = Actor "B" True False
c = Actor "C" False False
d = Actor "D" False True

actors :: [Actor]
actors = [a,b,c,d]

edges :: [Edge]
edges =
  [ Edge a c 1 2
  , Edge b d 2 2
  , Edge d c 1 1
  , Edge c d 1 1
  ]

----------------------------------------------------------
-- Topology matrix and null space calculation
----------------------------------------------------------

-- | Build the topology matrix, where rows are edges and columns are actors
-- 
-- Topo building logic:
-- - If actor is the source of the edge, the value is the production rate
-- - If actor is the destination of the edge, the value is the negative consumption rate
-- - Otherwise, the value is 0
-- 
buildTopologyMatrixEdgesRows :: [Actor] -> [Edge] -> Matrix R
buildTopologyMatrixEdgesRows actors edges =
  fromLists [ [ topo actor edge | actor <- actors ] | edge <- edges ]
  where
    topo actor edge
      | src edge == actor = fromIntegral (prod edge)
      | dst edge == actor = - fromIntegral (cons edge)
      | otherwise  = 0


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
      epsilon = 1e-9  -- tolerance for approxRational
      components = toList v

      -- make the first non-zero entry positive (or leave zeros if all zero)
      signedComponents =
        case dropWhile (\x -> abs x < epsilon) components of
          []    -> components
          (x:_) -> if x < 0 then map negate components else components

      -- approximate each component as a Rational within epsilon
      rationals = map (`approxRational` epsilon) signedComponents

      -- scale up by lcm of denominators to get integer vector
      denominators = map denominator rationals
      lcmDenominator = if null denominators then 1 else foldl1 lcm denominators
      ints = [ numerator r * (lcmDenominator `div` denominator r) | r <- rationals ]

      -- divide by gcd of non-zero entries to make the vector minimal
      nonZeroInts = filter (/= 0) ints
      gcdAll = if null nonZeroInts then 1 else foldl1 gcd (map abs nonZeroInts)
  in map (`div` gcdAll) ints


-- | Print the matrix with edges as rows and actors as columns
printMatrixEdgesRows :: [Edge] -> [Actor] -> Matrix R -> IO ()
printMatrixEdgesRows edges actors matrix = do
  -- print the header
  putStrLn $ "        " ++ unwords [ pad 10 (name actor) | actor <- actors ]
  -- print the rows
  mapM_ printRow (zip edges (toLists matrix))
  where
    pad width str = take width (str ++ repeat ' ')
    edgeLabel (Edge src dst _ _) = name src ++ "→" ++ name dst
    printRow (edge, rowValues) =
      putStrLn $ pad 8 (edgeLabel edge) ++ concatMap (pad 10 . show) rowValues

----------------------------------------------------------
-- Scheduler: Greedy + Forced approach with special handling for inputs/outputs.
-- Actors marked as isInput=True can always fire (external tokens available).
-- Actors marked as isOutput=True have no token constraints on outputs.
----------------------------------------------------------

-- | Get all the incoming edges for a given actor
-- Parameters:
-- - actors: list of actors
-- - edges: list of edges
-- - actorIndex: index of the actor
-- Returns: all edges satisfying: dst edge == actors[actorIndex]
incomingEdgeIndices :: [Actor] -> [Edge] -> Int -> [Int]
incomingEdgeIndices actors edges actorIndex = 
  [ idx | (idx, edge) <- zip [0..] edges, dst edge == (actors !! actorIndex) ]

-- | Get all the outgoing edges for a given actor
-- Parameters:
-- The same as incomingEdgeIndices
-- Returns: all edges satisfying: src edge == actors[actorIndex]
outgoingEdgeIndices :: [Actor] -> [Edge] -> Int -> [Int]
outgoingEdgeIndices actors edges actorIndex = 
  [ idx | (idx, edge) <- zip [0..] edges, src edge == (actors !! actorIndex) ]

-- | Update the element at a given index with a given function
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt index func list =
  let (before, element:after) = splitAt index list
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
fireOnce :: [Actor] -> [Edge] -> Int -> [Int] -> [Int] -> ([Int], [Int])
fireOnce actors edges actorIndex tokens minTokens =
  let incomingEdges = incomingEdgeIndices actors edges actorIndex 
      outgoingEdges = outgoingEdgeIndices actors edges actorIndex 
      tokensAfterConsume = foldl consume tokens incomingEdges
      tokensAfterProduce = foldl produce tokensAfterConsume outgoingEdges
      newMinTokens = zipWith min minTokens tokensAfterProduce
  in (tokensAfterProduce, newMinTokens)
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
-- Return: (schedule sequence, required initial token count)
--
-- Steps:
-- 1. Find all the fireable actors (repetition count > 0 and has enough input tokens)
-- 2. Fire all the fireable actors with the sequence order (could be optimized for minimal buffer size)
-- 3. If there is no fireable actor, force fire an actor with remaining times (to determine the initial token count)
-- 4. Record the minimum token value of each edge during the execution
-- 5. If there are negative minimum token values, it will be the required initial token count
--
greedyForcedSchedule :: [Actor] -> [Edge] -> [Int] -> ([Int], [Int])
greedyForcedSchedule actors edges repetitionCounts =
  let nActors = length actors
      nEdges = length edges
      initialTokens = replicate nEdges 0      -- Initial tokens are all 0
      initialMinTokens = replicate nEdges 0   -- Initialize the minimum token record
      
      -- Recursive worker function: 
      -- - remainingReps: remaining times
      -- - currentTokens: current token state
      -- - minTokens: historical minimum token
      -- - accSchedule: accumulated schedule sequence
      worker :: [Int] -> [Int] -> [Int] -> [Int] -> ([Int], [Int])
      worker remainingReps currentTokens minTokens accSchedule
        | sum remainingReps == 0 = (reverse accSchedule, minTokens)
        | otherwise =
            let isFireable i =
                  (remainingReps !! i) > 0 &&  -- has remaining fire repetitions
                  let incomingEdges = incomingEdgeIndices actors edges i
                      actor = actors !! i
                      canFire = case incomingEdges of
                          [] -> 
                            if isInput actor
                              then True    -- If is input actor and no incoming edges from other actors
                              else error $ "Invalid graph: actor " ++ name actor ++ " has no incoming edges but is not marked as input."
                          _  -> all (\edgeIdx -> -- All incoming edges must have enough tokens
                                  currentTokens !! edgeIdx >= cons (edges !! edgeIdx)
                                ) incomingEdges
                  in canFire


                fireableActors = [i | i <- [0..nActors-1], isFireable i]
            
              in case fireableActors of
                  (actorIdx:_) ->  -- If there is a fireable actor, fire the first one 
                    let (newTokens, newMinTokens) = fireOnce actors edges actorIdx currentTokens minTokens
                        newRemaining = updateAt actorIdx (subtract 1) remainingReps
                    in worker newRemaining newTokens newMinTokens (actorIdx:accSchedule)
                  [] ->  -- If there is no fireable actor, force fire an actor with remaining times
                    case findIndex (>0) remainingReps of
                      -- "Nothing" should not happen, since we have already checked sum remainingReps != 0
                      Nothing -> (reverse accSchedule, minTokens)
                      Just forcedIdx ->
                        let (newTokens, newMinTokens) = fireOnce actors edges forcedIdx currentTokens minTokens  
                            newRemaining = updateAt forcedIdx (subtract 1) remainingReps
                        in worker newRemaining newTokens newMinTokens (forcedIdx:accSchedule)
  
  in
    let (schedule, minTokens) = worker repetitionCounts initialTokens initialMinTokens []
        -- Calculate the initial tokens required to make the schedule valid
        initialTokensRequired = map (\minVal -> max 0 (-minVal)) minTokens
    in (schedule, initialTokensRequired)

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
  let 
      -- Recursively check the schedule
      check :: [Int] -> [Int] -> Bool
      check currentTokens [] = True  -- Base case: empty schedule is valid
      check currentTokens (actorIndex:remainingSchedule) =
        let incomingEdges = incomingEdgeIndices actors edges actorIndex
            
            -- Simulate consumption and check if it is valid
            tokensAfterConsume = foldl consume currentTokens incomingEdges
            consumptionValid = all (>=0) tokensAfterConsume
            
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
-- Main
-- Run the whole process and print each step
----------------------------------------------------------

main :: IO ()
main = do
  let mat = buildTopologyMatrixEdgesRows actors edges
  putStrLn "Topology Matrix (Edges x Actors):"
  printMatrixEdgesRows edges actors mat

  let rankMat = rank mat
  putStrLn $ "\nMatrix Rank: " ++ show rankMat
  putStrLn $ "Number of actors: " ++ show (length actors)
  putStrLn $ "Number of edges: " ++ show (length edges)

  if rankMat == length actors - 1 then do
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
    let actorLabels = [ name a | a <- actors ]
    putStrLn $ intercalate " : " [ label ++ "=" ++ show r | (label,r) <- zip actorLabels repInt ]

    let repCounts = map fromIntegral repInt :: [Int]
        (schedIdxs, requiredInitial) = greedyForcedSchedule actors edges repCounts
        schedNames = map (name . (actors !!)) schedIdxs

    putStrLn "\nGenerated schedule (actor firing order):"
    putStrLn $ intercalate ", " schedNames

    putStrLn "\nRequired initial tokens (per edge) to make this schedule valid):"
    let edgeLabels = [ name (src e) ++ "→" ++ name (dst e) | e <- edges ]
    mapM_ (\(lbl, t) -> putStrLn $ "  " ++ lbl ++ ": " ++ show t) (zip edgeLabels requiredInitial)

    let ok = verifySchedule actors edges requiredInitial schedIdxs repCounts
    putStrLn $ "\nVerification of schedule with computed initial tokens: " ++ (if ok then "OK" else "FAILED")

    ------------------------------------------------------
    -- Simulate buffer usage for one period
    ------------------------------------------------------
    let bufSizes = simulateBufferUsage actors edges requiredInitial schedIdxs
    putStrLn "\nSimulated buffer sizes (maximum tokens observed per edge):"
    mapM_ (\(lbl, sz) -> putStrLn $ "  " ++ lbl ++ ": " ++ show sz) (zip edgeLabels bufSizes)

  else
    putStrLn "\nMatrix rank is not equal to number of actors minus one. Cannot compute repetition vector."