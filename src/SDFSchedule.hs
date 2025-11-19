{-# LANGUAGE DeriveGeneric #-}

module SDFSchedule (computeScheduleAndBuffers, computeScheduleAndBuffersPrint) where

import Data.List (find, intercalate, nub)
import Data.Ratio (approxRational, denominator, numerator)
import ForSyDeIR
import GHC.Generics (Generic)
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
            signalId
            (findActorByName srcId)
            (findActorByName dstId)
            prodRate
            consRate
            False
            0
        | IRSignal signalId (srcId, prodRate) (dstId, consRate) <- signals,
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
                    IRDelay dName _ (_, _) -> dName == delayName
                    IRActor _ _ _ _ -> False
                )
                constructors of
                Just (IRDelay _ delayInitTokens (_, _)) -> delayInitTokens
                Just (IRActor _ _ _ _) ->
                  error $ "Expected delay node but found actor: " ++ delayName -- To eliminate pattern match warning
                Nothing -> error $ "Delay node " ++ delayName ++ " not found in constructors"
            incoming =
              [ (signalId, srcId, prodRate)
              | IRSignal signalId (srcId, prodRate) (dstId, _) <- signals,
                dstId == delayName
              ]
            outgoing =
              [ (signalId, dstId, consRate)
              | IRSignal signalId (srcId, _) (dstId, consRate) <- signals,
                srcId == delayName
              ]
         in case (incoming, outgoing) of
              ([(inSignalId, srcIn, prodIn)], [(outSignalId, dstOut, consOut)]) ->
                [ Edge
                    (inSignalId ++ "_" ++ outSignalId) -- delay edge names are combined
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
      normalizeSelfLoop e@(Edge edgeNameValue srcActor dstActor prodRate consRate _ _)
        | name srcActor == name dstActor =
            if prodRate /= consRate
              then
                error $
                  "Invalid self-loop on actor "
                    ++ name srcActor
                    ++ " (edge: "
                    ++ edgeNameValue
                    ++ ")"
                    ++ ": prod="
                    ++ show prodRate
                    ++ ", cons="
                    ++ show consRate
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
  { edgeName :: String,
    src :: Actor,
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

-- | Return the matrix as a string with edges as rows and actors as columns
matrixEdgesRowsToString :: [Edge] -> [Actor] -> Matrix R -> String
matrixEdgesRowsToString edges actors topoMatrix =
  let -- Calculate the maximum width of each column
      actorNameWidths = map (length . name) actors
      rowValueWidths = map (maximum . map (length . show)) (toLists topoMatrix)
      colWidths = zipWith max actorNameWidths rowValueWidths
      totalColWidths = map (+ 2) colWidths -- Add 2 to each column width for spacing

      -- Calculate the maximum width of the edge labels
      edgeLabelWidth = maximum (map (length . edgeLabel) edges)

      -- Build the header
      header =
        pad (edgeLabelWidth + 2) "Edge\\Actor"
          ++ concatMap (\(actor, width) -> pad width (name actor)) (zip actors totalColWidths)

      -- Build the separator line
      totalWidth = edgeLabelWidth + 2 + sum totalColWidths
      separator = replicate totalWidth '-'

      -- Build the matrix rows
      matrixRows = map (rowToString edgeLabelWidth totalColWidths) (zip edges (toLists topoMatrix))

      -- Combine everything
      result =
        "Topology Matrix (Edges × Actors):\n"
          ++ header
          ++ "\n"
          ++ separator
          ++ "\n"
          ++ unlines matrixRows
   in result
  where
    pad width str = take width (str ++ repeat ' ')
    edgeLabel edge = edgeName edge

    rowToString labelWidth colWidths (edge, rowValues) =
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
  case splitAt index list of
    (before, element : after) -> before ++ (func element) : after
    (_, []) -> error $ "updateAt: index " ++ show index ++ " out of bounds for list of length " ++ show (length list)

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

simulateBufferUsage :: [Actor] -> [Edge] -> [Int] -> [Int] -> [(String, Int)]
simulateBufferUsage actors edges initialTokens schedule =
  let -- Get all edge names
      edgeNames = map edgeName edges

      -- Simulate one step：fire one actor, update token state and max buffer record
      simulateStep (currentTokens, maxBuffer) actorIndex =
        let incomingEdges = incomingEdgeIndices actors edges actorIndex
            outgoingEdges = outgoingEdgeIndices actors edges actorIndex
            tokensAfterConsume = foldl consume currentTokens incomingEdges
            tokensAfterProduce = foldl produce tokensAfterConsume outgoingEdges
            newMaxBuffer = zipWith max maxBuffer tokensAfterProduce
         in (tokensAfterProduce, newMaxBuffer)

      -- Use foldl to simulate the whole schedule
      (_, finalMaxBuffer) = foldl simulateStep (initialTokens, initialTokens) schedule

      -- Combine buffer sizes and names
      bufferSizesWithNames = zip edgeNames finalMaxBuffer
   in bufferSizesWithNames
  where
    consume tokens edgeIdx =
      let consumption = cons (edges !! edgeIdx)
       in updateAt edgeIdx (subtract consumption) tokens
    produce tokens edgeIdx =
      let production = prod (edges !! edgeIdx)
       in updateAt edgeIdx (+ production) tokens

computeIOBufferSizes :: IRSystem -> [(String, Int)] -> [(String, Int)]
computeIOBufferSizes (IRSystem (inputs, outputs) _ signals _) repsWithNames =
  let -- Find all external input edges
      inputEdges =
        [ (signalId, dstRate, findActorRep dstId)
        | IRSignal signalId (srcId, _) (dstId, dstRate) <- signals,
          srcId `elem` inputs
        ]

      -- Find all external output edges
      outputEdges =
        [ (signalId, srcRate, findActorRep srcId)
        | IRSignal signalId (srcId, srcRate) (dstId, _) <- signals,
          dstId `elem` outputs
        ]

      -- Find repetition count by actor names
      findActorRep actorName =
        case lookup actorName repsWithNames of
          Just rep -> rep
          Nothing -> error $ "Actor " ++ actorName ++ " not found in repetition counts"

      -- Calculate input buffer size：rate × dst actor rep count
      inputBuffers =
        [ (signalId, dstRate * rep)
        | (signalId, dstRate, rep) <- inputEdges
        ]

      -- Calculate output buffer size：rate × src actor rep count
      outputBuffers =
        [ (signalId, srcRate * rep)
        | (signalId, srcRate, rep) <- outputEdges
        ]
   in inputBuffers ++ outputBuffers

----------------------------------------------------------
-- Verification of the schedule
----------------------------------------------------------

verifySchedule :: [Actor] -> [Edge] -> [Int] -> [Int] -> [Int] -> Bool
verifySchedule actors edges initialTokens schedule _repetitionCounts =
  let -- Recursively check the schedule
      check :: [Int] -> [Int] -> Bool
      check _ [] = True -- Base case: empty schedule is valid
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
computeScheduleAndBuffers :: IRSystem -> ([String], [(String, Int)])
computeScheduleAndBuffers irSystem =
  let (actors, edges) = convertIRSystem irSystem
   in if null edges
        then
          -- If there are no internal edges, fire all actor once, no buffer required
          let schedNames = map name actors
              repsWithNames = zip (map name actors) (replicate (length actors) 1)
              internalBufSizes = zip (map edgeName edges) (replicate (length edges) 0)
              ioBufSizes = computeIOBufferSizes irSystem repsWithNames
           in (schedNames, ioBufSizes ++ internalBufSizes)
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

                      finalResult =
                        if not isZeroVector
                          then error "Verification failed: repetition vector is not in null space"
                          else
                            let repCounts = map fromIntegral repInt :: [Int]
                                repsWithNames = zip (map name actors) repCounts
                                schedIdxs = greedySchedule actors edges repCounts
                                schedNames = map (name . (actors !!)) schedIdxs
                                internalBufSizes = simulateBufferUsage actors edges (map initTokens edges) schedIdxs
                                ioBufSizes = computeIOBufferSizes irSystem repsWithNames
                             in (schedNames, ioBufSizes ++ internalBufSizes)
                   in finalResult
                else
                  error "Matrix rank is not equal to number of actors minus one. Cannot compute repetition vector."

----------------------------------------------------------
-- Pretty-print version
----------------------------------------------------------

-- | Returns a string with topology matrix, repetition vector, schedule, verification results, and buffer usage.
computeScheduleAndBuffersPrint :: IRSystem -> String
computeScheduleAndBuffersPrint irSystem =
  let (actors, edges) = convertIRSystem irSystem
   in if null edges
        then
          -- If there are no internal edges, fire all actor once, no buffer required
          let schedNames = map name actors
              repsWithNames = zip (map name actors) (replicate (length actors) 1)
              internalBufSizes = zip (map edgeName edges) (replicate (length edges) 0)
              ioBufSizes = computeIOBufferSizes irSystem repsWithNames
              allBufSizes = ioBufSizes ++ internalBufSizes
           in "No internal edges found.\n\n"
                ++ "Schedule (all actors fire once):\n"
                ++ intercalate ", " schedNames
                ++ "\n\nI/O buffer sizes:"
                ++ concatMap
                  (\(edgeNameVal, sz) -> "\n  " ++ edgeNameVal ++ ": " ++ show sz)
                  allBufSizes
        else
          let mat = buildTopologyMatrixEdgesRows actors edges
              matrixStr = matrixEdgesRowsToString edges actors mat

              rankMat = rank mat
              header =
                matrixStr
                  ++ "\n\nMatrix Rank: "
                  ++ show rankMat
                  ++ "\nNumber of actors: "
                  ++ show (length actors)
                  ++ "\nNumber of edges: "
                  ++ show (length edges)
           in if rankMat == length actors - 1
                then
                  let ns = computeNullSpace mat
                      repVec = flatten (takeColumns 1 ns)
                      repInt = normalizeToInteger repVec

                      -- Verification: multiply the integer repetition vector back to the topology matrix
                      repVector = fromIntegral <$> repInt :: [R]
                      verificationResult = mat #> vector repVector
                      isZeroVector = all (\x -> abs x < 1e-9) (LinearAlgebra.toList verificationResult)

                      nullSpaceStr =
                        "\n\nNull Space (fractional repetition vector for actors):\n"
                          ++ dispToString 4 ns

                      verificationStr =
                        "\n\nVerification of null space vector:"
                          ++ "\nTopology Matrix × Repetition Vector ≈ Zero Vector? "
                          ++ show isZeroVector
                          ++ if not isZeroVector
                            then
                              "\nWarning: Product is not zero!\nProduct vector:\n"
                                ++ show verificationResult
                            else ""

                      repVecStr =
                        "\n\nNormalized repetition vector for ACTORS (integers):"
                          ++ intercalate "\n" [label ++ "=" ++ show r | (label, r) <- zip (map name actors) repInt]

                      repCounts = map fromIntegral repInt :: [Int]
                      repsWithNames = zip (map name actors) repCounts
                      schedIdxs = greedySchedule actors edges repCounts
                      schedNames = map (name . (actors !!)) schedIdxs

                      schedStr =
                        "\n\nGenerated schedule (actor firing order):\n"
                          ++ intercalate ", " schedNames

                      initialTokensStr =
                        "\n\nInitial tokens (provided by IR):"
                          ++ concatMap
                            (\(ename, e) -> "\n " ++ ename ++ ": " ++ show (initTokens e))
                            (zip (map edgeName edges) edges)

                      ok = verifySchedule actors edges (map initTokens edges) schedIdxs repCounts
                      verificationSchedStr =
                        "\n\nVerification of schedule with computed initial tokens: "
                          ++ (if ok then "OK" else "FAILED")

                      -- Simulate buffer usage for one period
                      internalBufSizes = simulateBufferUsage actors edges (map initTokens edges) schedIdxs
                      ioBufSizes = computeIOBufferSizes irSystem repsWithNames
                      allBufSizes = ioBufSizes ++ internalBufSizes

                      internalBufStr =
                        "\n\nInternal buffer sizes (maximum tokens observed per edge):"
                          ++ concatMap
                            (\(ename, sz) -> "\n  " ++ ename ++ ": " ++ show sz)
                            internalBufSizes

                      ioBufStr =
                        "\n\nI/O buffer sizes (rate × repetition count):"
                          ++ concatMap
                            (\(ename, sz) -> "\n  " ++ ename ++ ": " ++ show sz)
                            ioBufSizes

                      allBufStr =
                        "\n\nAll buffer sizes (I/O + internal):"
                          ++ concatMap
                            (\(ename, sz) -> "\n  " ++ ename ++ ": " ++ show sz)
                            allBufSizes
                   in header
                        ++ nullSpaceStr
                        ++ verificationStr
                        ++ repVecStr
                        ++ schedStr
                        ++ initialTokensStr
                        ++ verificationSchedStr
                        ++ internalBufStr
                        ++ ioBufStr
                        ++ allBufStr
                else
                  header ++ "\n\nMatrix rank is not equal to number of actors minus one. Cannot compute repetition vector."

-- Helper function to convert matrix display to string
dispToString :: Int -> Matrix R -> String
dispToString digits mat =
  let matrixRows = toLists mat
      formattedRows = map (map (formatNumber digits)) matrixRows
   in unlines (map (intercalate "  " . map (pad 10)) formattedRows)
  where
    formatNumber _ x
      | abs x < 1e-12 = "0"
      | denominator rat == 1 = show (numerator rat)
      | otherwise = show (numerator rat) ++ "/" ++ show (denominator rat)
      where
        rat = approxRational x (1e-12)

    pad width str = take width (str ++ repeat ' ')
