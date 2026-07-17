{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module SDFSchedule (Schedule (..), computeScheduleAndBuffers, computeScheduleAndBuffersPrint) where

import Data.List (elemIndex, find, intercalate, nub)
import Data.Ratio (denominator, numerator)
import ForSyDeIR
import GHC.Generics (Generic)

-- | Convert ForSyDe IR to SDF data structures
convertIRSystem :: IRSystem -> Either String ([Actor], [Edge])
convertIRSystem (IRSystem (inputNames, outputNames) constructors signals _) = do
  let -- 1. Delay node names (IRDelay has two parameters)
      delayNames = [n | IRDelay n _ (_, _) <- constructors]

      -- 2. All actor names (excluding delays)
      allActorNames =
        [ n
        | IRActor n _ _ (_, _) <- constructors
        ]

      -- 3. Actors that receive from inputs
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
        case find (\a -> name a == n) baseActors of
          Just a -> pure a
          Nothing -> Left ("Actor not found: " ++ show n)

      -- Non-delay, non-input/output signals
      normalSigs = filter f signals
        where
          f (IRSignal _ (srcId, _) (dstId, _)) =
            srcId `notElem` inputNames
              && dstId `notElem` outputNames
              && srcId `notElem` delayNames
              && dstId `notElem` delayNames

      makeNormalEdges
        (IRSignal signalId (srcId, prodRate) (dstId, consRate)) = do
          srcActor <- findActorByName srcId
          dstActor <- findActorByName dstId
          pure $ Edge signalId srcActor dstActor prodRate consRate False 0 Nothing

      makeDelayEdge delayName =
        let getDelayToken acc e =
              case (e, acc) of
                (IRDelay dName tokens _, Left _) | dName == delayName -> pure tokens
                (_, a) -> a
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
              -- input is a global system input -> ignore
              ([(inSignalId, _, _)], _) | inSignalId `elem` inputNames -> pure []
              -- output is a global system output -> ignore
              (_, [(outSignalId, _, _)]) | outSignalId `elem` outputNames -> pure []
              ([(inSignalId, srcIn, prodIn)], [(outSignalId, dstOut, consOut)]) -> do
                srcActor <- findActorByName srcIn
                dstActor <- findActorByName dstOut
                delayTokens <-
                  foldl'
                    getDelayToken
                    (Left $ "Delay node " ++ show delayName ++ " not found")
                    constructors
                pure
                  [ Edge
                      -- use the input signal id as the delay edge id
                      inSignalId
                      srcActor
                      dstActor
                      prodIn
                      consOut
                      True
                      (length delayTokens)
                      -- allow the edge to be found by the output signal id as well.
                      -- Note that we need the in->in as we only lookup the aliaes.
                      (Just [(inSignalId, inSignalId), (outSignalId, inSignalId)])
                  ]
              ([], _) ->
                Left $ "Delay node " ++ show delayName ++ " has no input signal."
              (_, []) ->
                Left $ "Delay node " ++ show delayName ++ " has no output signal."
              _ ->
                Left $ "Delay node " ++ show delayName ++ " must have exactly one input and one output."

      -- 8. Normalize self-loops: ensure prod == cons, otherwise error
      normalizeSelfLoop e@(Edge edgeNameValue srcActor dstActor prodRate consRate _ _ _)
        | name srcActor == name dstActor =
            if prodRate /= consRate
              then
                Left $
                  "Invalid self-loop on actor "
                    ++ show (name srcActor)
                    ++ " (edge: "
                    ++ show edgeNameValue
                    ++ ")"
                    ++ ": prod="
                    ++ show prodRate
                    ++ ", cons="
                    ++ show consRate
              else pure e -- prod == cons is valid
        | otherwise = pure e

  -- Folded delay edges (A -> delay -> B → becomes one edge)
  delayEdges <- mconcat <$> (sequence $ map makeDelayEdge delayNames)
  normalEdges <- sequence $ map makeNormalEdges normalSigs
  finalEdges <- sequence $ map normalizeSelfLoop (normalEdges ++ delayEdges)
  pure (baseActors, finalEdges)

----------------------------------------------------------
-- Data structures definitions
----------------------------------------------------------

data Actor = Actor
  { name :: IRId,
    isInput :: Bool
  }
  deriving (Show, Eq, Generic)

data Edge = Edge
  { edgeName :: IRId,
    src :: Actor,
    dst :: Actor,
    prod :: Int,
    cons :: Int,
    isDelay :: Bool,
    initTokens :: Int, -- Count of Init tokens for delay edges
    buffers :: Maybe [(IRId, IRId)]
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
buildTopologyMatrixEdgesRows :: [Actor] -> [Edge] -> [[Integer]]
buildTopologyMatrixEdgesRows actors edges =
  [[topo actor edge | actor <- actors] | edge <- edges]
  where
    topo actor edge
      | src edge == dst edge = 0
      | src edge == actor = fromIntegral (prod edge)
      | dst edge == actor = -fromIntegral (cons edge)
      | otherwise = 0

-- | Reduced row echelon form via exact Gaussian elimination over the rationals.
-- Returns the reduced rows and the pivot column indices, in pivot order.
rowReduce :: [[Rational]] -> ([[Rational]], [Int])
rowReduce rowsIn = go rowsIn 0 []
  where
    nCols = case rowsIn of
      [] -> 0
      (r : _) -> length r
    go rows col pivots
      | col >= nCols = (rows, reverse pivots)
      | otherwise =
          let (done, rest) = splitAt (length pivots) rows
           in case break (\r -> r !! col /= 0) rest of
                (_, []) -> go rows (col + 1) pivots
                (above, pivotRow : below) ->
                  let normalized = map (/ (pivotRow !! col)) pivotRow
                      eliminate r =
                        let factor = r !! col
                         in if factor == 0
                              then r
                              else zipWith (\a b -> a - factor * b) r normalized
                   in go
                        (map eliminate done ++ [normalized] ++ map eliminate (above ++ below))
                        (col + 1)
                        (col : pivots)

matrixRank :: [[Integer]] -> Int
matrixRank = length . snd . rowReduce . map (map fromInteger)

-- | Basis vectors of the null space of an integer matrix, as exact rationals.
-- One basis vector per free column of the reduced matrix.
nullspaceBasis :: [[Integer]] -> [[Rational]]
nullspaceBasis mat =
  let (rref, pivots) = rowReduce (map (map fromInteger) mat)
      nCols = case mat of
        [] -> 0
        (r : _) -> length r
      freeCols = [c | c <- [0 .. nCols - 1], c `notElem` pivots]
      entry freeCol col
        | col == freeCol = 1
        | otherwise = case elemIndex col pivots of
            Just i -> negate (rref !! i !! freeCol)
            Nothing -> 0
   in [[entry f c | c <- [0 .. nCols - 1]] | f <- freeCols]

-- | Scale an exact rational vector to the smallest integer vector pointing in
-- the same direction. An all-negative vector is flipped to positive.
toMinimalIntegers :: [Rational] -> [Integer]
toMinimalIntegers xs =
  let commonDenom = foldl' lcm 1 (map denominator xs)
      ints = [numerator x * (commonDenom `div` denominator x) | x <- xs]
      gcdAll = foldl' gcd 0 ints
      reduced = if gcdAll == 0 then ints else map (`div` gcdAll) ints
   in if all (< 0) reduced then map negate reduced else reduced

multiplyMatrixVector :: [[Integer]] -> [Integer] -> [Integer]
multiplyMatrixVector mat vec = map (sum . zipWith (*) vec) mat

-- | Return the matrix as a string with edges as rows and actors as columns
matrixEdgesRowsToString :: [Edge] -> [Actor] -> [[Integer]] -> String
matrixEdgesRowsToString edges actors topoMatrix =
  let -- Calculate the maximum width of each column
      actorNameWidths = map (length . show . name) actors
      rowValueWidths = map (maximum . map (length . show)) topoMatrix
      colWidths = zipWith max actorNameWidths rowValueWidths
      totalColWidths = map (+ 2) colWidths -- Add 2 to each column width for spacing

      -- Calculate the maximum width of the edge labels
      edgeLabelWidth = maximum (map (length . show . edgeLabel) edges)

      -- Build the header
      header =
        pad (edgeLabelWidth + 2) "Edge\\Actor"
          ++ concatMap (\(actor, width) -> pad width (show $ name actor)) (zip actors totalColWidths)

      -- Build the separator line
      totalWidth = edgeLabelWidth + 2 + sum totalColWidths
      separator = replicate totalWidth '-'

      -- Build the matrix rows
      matrixRows = map (rowToString edgeLabelWidth totalColWidths) (zip edges topoMatrix)

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
      pad (labelWidth + 2) (show $ edgeLabel edge)
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
greedySchedule :: [Actor] -> [Edge] -> [Int] -> Either String [Int]
greedySchedule actors edges repetitionCounts =
  let nActors = length actors
      initialTokens = map initTokens edges -- map initial tokens

      -- Recursive worker function:
      -- - remainingReps: remaining times
      -- - currentTokens: current token state
      -- - accSchedule: accumulated schedule sequence
      worker :: [Int] -> [Int] -> [Int] -> Either String [Int]
      worker remainingReps currentTokens accSchedule
        | sum remainingReps == 0 = pure $ reverse accSchedule
        | otherwise =
            let isFireable i = do
                  let incomingEdges = incomingEdgeIndices actors edges i -- has remaining fire repetitions
                      actor = actors !! i
                      canFire = case incomingEdges of
                        [] ->
                          if isInput actor
                            then pure $ True -- If is input actor and no incoming edges from other actors
                            else Left $ "Invalid graph: actor " ++ show (name actor) ++ " has no incoming edges but is not marked as input."
                        _ ->
                          Right $
                            all
                              ( \edgeIdx ->
                                  -- All incoming edges must have enough tokens
                                  currentTokens !! edgeIdx >= cons (edges !! edgeIdx)
                              )
                              incomingEdges
                   in do
                        firable <- canFire
                        pure $ (i, firable && (remainingReps !! i) > 0)
                -- fireableActors = [i | i <- [0 .. nActors - 1], isFireable i]
                fireableActors = map fst <$> filter (\(_, v) -> v) <$> sequence (map isFireable [0 .. nActors - 1])
             in case fireableActors of
                  Right (actorIdx : _) ->
                    -- If there is a fireable actor, fire the first one
                    let newTokens = fireOnce actors edges actorIdx currentTokens
                        newRemaining = updateAt actorIdx (subtract 1) remainingReps
                     in worker newRemaining newTokens (actorIdx : accSchedule)
                  Right [] -> Left "Error: Deadlock detected, cannot find fireable actor.\n"
                  Left e -> Left e
   in worker repetitionCounts initialTokens []

----------------------------------------------------------
-- Simulation to compute buffer size per edge
-- After generating a valid schedule, simulate it once over one period
-- The buffer size of each edge is defined as the maximum token count observed
----------------------------------------------------------

simulateBufferUsage :: [Actor] -> [Edge] -> [Int] -> [Int] -> [(IRId, Int)]
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

computeIOBufferSizes :: IRSystem -> [(IRId, Int)] -> Either String ([(IRId, Int)], [(IRId, IRId)])
computeIOBufferSizes (IRSystem (inputs, outputs) constructors signals _) repsWithNames =
  let -- Find all external input edges
      inputSignals = [s | s@(IRSignal _ (srcId, _) (_, _)) <- signals, srcId `elem` inputs]
      inputEdges = foldr f (Right []) $ map nonDelayInputSignal inputSignals
        where
          f el acc = do
            a <- acc
            (IRSignal signalId (srcId, _) (dstId, dstRate), aliases, nTokens) <- el
            if srcId `elem` inputs
              then do
                dstReps <- findActorRep dstId
                pure $ (signalId, dstRate, dstReps, aliases, nTokens) : a
              else acc

      -- Find the first non-delay process connected to the input signal
      nonDelayInputSignal s@(IRSignal sigId (srcId, srcRate) (dstId, _)) =
        case find (\c -> constructorName c == dstId) constructors of
          -- The signal directly connects to an actor
          Just (IRActor _ _ _ _) -> pure (s, [], 0)
          -- The signal connects to a delay, keep going
          Just (IRDelay dName tokens (_, next)) | dstId == dName ->
            case find (\(IRSignal sId _ _) -> sId == next) signals of
              Just (IRSignal sigAlias (_, _) (newDstId, newDstRate)) -> do
                (sig, aliases, nTokens) <- nonDelayInputSignal (IRSignal sigId (srcId, srcRate) (newDstId, newDstRate))
                pure (sig, (sigId, sigId) : (sigAlias, sigId) : aliases, nTokens + length tokens)
              Nothing -> Left $ "Could not find next signal " ++ show next
          _ -> Left $ "Could not find the process " ++ show srcId

      -- Find all external output edges
      outputSignals = [s | s@(IRSignal _ (_, _) (dstId, _)) <- signals, dstId `elem` outputs]
      outputEdges = foldr f (Right []) $ map nonDelayOutputSignal outputSignals
        where
          f el acc = do
            a <- acc
            (IRSignal signalId (srcId, srcRate) (dstId, _), aliases, nTokens) <- el
            if dstId `elem` outputs
              then do
                srcReps <- findActorRep srcId
                pure $ (signalId, srcRate, srcReps, aliases, nTokens) : a
              else acc

      -- Find the first non-delay process connected to the output signal
      nonDelayOutputSignal s@(IRSignal sigId (srcId, _) (dstId, dstRate)) =
        case find (\c -> constructorName c == srcId) constructors of
          -- The signal directly connects to an actor
          Just (IRActor _ _ _ _) -> pure (s, [], 0)
          -- The signal connects to a delay, keep going
          Just (IRDelay dName tokens (next, _)) | srcId == dName ->
            case find (\(IRSignal sId _ _) -> sId == next) signals of
              Just (IRSignal sigAlias (newSrcId, newSrcRate) (_, _)) -> do
                (sig, aliases, nTokens) <- nonDelayOutputSignal (IRSignal sigId (newSrcId, newSrcRate) (dstId, dstRate))
                pure (sig, (sigId, sigId) : (sigAlias, sigId) : aliases, nTokens + length tokens)
              Nothing -> Left $ "Could not find next signal " ++ show next
          _ -> Left $ "Could not find the process " ++ show srcId

      constructorName = \case
        IRActor cName _ _ _ -> cName
        IRDelay cName _ _ -> cName

      -- Find repetition count by actor names
      findActorRep actorName =
        case lookup actorName repsWithNames of
          Just rep -> pure rep
          Nothing -> Left $ "Actor " ++ show actorName ++ " not found in repetition counts"

      -- separate the buffers and aliases
      foldBuffer (sigId, rate, rep, aliases, nTokens) (sigAcc, aliasAcc) =
        ((sigId, max (rate * rep) nTokens) : sigAcc, aliases ++ aliasAcc)
   in do
        inEdges <- inputEdges
        outEdges <- outputEdges
        pure $ foldr foldBuffer ([], []) $ inEdges ++ outEdges

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

newtype Schedule = Schedule ([IRId], [(IRId, Int)], [(IRId, IRId)])
  deriving (Show)

-- | Returns schedule as actor names, buffer sizes, and delay buffer mappings
-- Returns: (schedule_order, [(buffer_name, buffer_size)], [(original_signal, delay_buffer_name)])
computeScheduleAndBuffers :: IRSystem -> Either String Schedule
computeScheduleAndBuffers irSystem = do
  (actors, edges) <- convertIRSystem irSystem
  if null edges
    then
      -- If there are no internal edges, fire all actor once, no buffer required
      let schedNames = map name actors
          repsWithNames = zip (map name actors) (replicate (length actors) 1)
          internalBufSizes = zip (map edgeName edges) (replicate (length edges) 0)
       in do
            (ioBufSizes, aliases) <- computeIOBufferSizes irSystem repsWithNames
            pure $ Schedule (schedNames, ioBufSizes ++ internalBufSizes, aliases)
    else
      let mat = buildTopologyMatrixEdgesRows actors edges
          rankMat = matrixRank mat
       in if rankMat == length actors - 1
            then case nullspaceBasis mat of
              [] -> Left "No repetition vector found: null space is empty."
              (basisVec : _) ->
                let repInt = toMinimalIntegers basisVec

                    -- Verification
                    isZeroVector = all (== 0) (multiplyMatrixVector mat repInt)

                    finalResult
                      | not isZeroVector = Left "Verification failed: repetition vector is not in null space"
                      | any (<= 0) repInt = Left "Cannot compute a strictly positive repetition vector for this graph."
                      | otherwise =
                          let repCounts = map fromIntegral repInt :: [Int]
                              repsWithNames = zip (map name actors) repCounts
                              delayBuffers = getBuffers edges
                           in do
                                schedIdxs <- greedySchedule actors edges repCounts
                                internalBufSizes <- pure $ simulateBufferUsage actors edges (map initTokens edges) schedIdxs
                                schedNames <- pure $ map (name . (actors !!)) schedIdxs
                                (ioBufSizes, aliases) <- computeIOBufferSizes irSystem repsWithNames
                                pure $ Schedule (schedNames, ioBufSizes ++ internalBufSizes, delayBuffers ++ aliases)
                 in finalResult
            else
              Left "Matrix rank is not equal to number of actors minus one. Cannot compute repetition vector."

getBuffers :: [Edge] -> [(IRId, IRId)]
getBuffers edges =
  concatMap
    ( \e ->
        case buffers e of
          Just bufList -> bufList
          Nothing -> []
    )
    edges

----------------------------------------------------------
-- Pretty-print version
----------------------------------------------------------

-- | Returns a string with topology matrix, repetition vector, schedule, verification results, and buffer usage.
-- Note: partial function
computeScheduleAndBuffersPrint :: IRSystem -> String
computeScheduleAndBuffersPrint irSystem =
  case convertIRSystem irSystem of
    Left e -> error e
    Right (actors, edges) ->
      let outputString =
            if null edges
              then
                -- If there are no internal edges, fire all actor once, no buffer required
                let schedNames = map name actors
                    repsWithNames = zip (map name actors) (replicate (length actors) 1)
                    internalBufSizes = zip (map edgeName edges) (replicate (length edges) 0)
                    ioBufSizes = case computeIOBufferSizes irSystem repsWithNames of
                      Left e -> error e
                      Right v -> v
                    allBufSizes = fst ioBufSizes ++ internalBufSizes
                 in "No internal edges found.\n\n"
                      ++ "Schedule (all actors fire once):\n"
                      ++ intercalate ", " (map show schedNames)
                      ++ "\n\nI/O buffer sizes:"
                      ++ concatMap
                        (\(edgeNameVal, sz) -> "\n  " ++ show edgeNameVal ++ ": " ++ show sz)
                        allBufSizes
              else
                let mat = buildTopologyMatrixEdgesRows actors edges
                    matrixStr = matrixEdgesRowsToString edges actors mat

                    rankMat = matrixRank mat
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
                        let basisVec = case nullspaceBasis mat of
                              (v : _) -> v
                              [] -> error "No repetition vector found: null space is empty."
                            repInt = toMinimalIntegers basisVec

                            -- Verification: multiply the integer repetition vector back to the topology matrix
                            verificationResult = multiplyMatrixVector mat repInt
                            isZeroVector = all (== 0) verificationResult

                            nullSpaceStr =
                              "\n\nNull Space (fractional repetition vector for actors):\n"
                                ++ nullspaceVectorToString basisVec

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
                                ++ intercalate "\n" [show label ++ "=" ++ show r | (label, r) <- zip (map name actors) repInt]

                            repCounts = map fromIntegral repInt :: [Int]
                            repsWithNames = zip (map name actors) repCounts
                            schedIdxs = case greedySchedule actors edges repCounts of
                              Left e -> error e
                              Right v -> v
                            schedNames = map (name . (actors !!)) schedIdxs

                            schedStr =
                              "\n\nGenerated schedule (actor firing order):\n"
                                ++ intercalate ", " (map show schedNames)

                            initialTokensStr =
                              "\n\nInitial tokens (provided by IR):"
                                ++ concatMap
                                  (\(ename, e) -> "\n " ++ ename ++ ": " ++ show (initTokens e))
                                  (zip (map (show . edgeName) edges) edges)

                            ok = verifySchedule actors edges (map initTokens edges) schedIdxs repCounts
                            verificationSchedStr =
                              "\n\nVerification of schedule with computed initial tokens: "
                                ++ (if ok then "OK" else "FAILED")

                            -- Simulate buffer usage for one period
                            internalBufSizes = simulateBufferUsage actors edges (map initTokens edges) schedIdxs
                            ioBufSizes = case computeIOBufferSizes irSystem repsWithNames of
                              Left e -> error e
                              Right v -> v
                            allBufSizes = fst ioBufSizes ++ internalBufSizes

                            internalBufStr =
                              "\n\nInternal buffer sizes (maximum tokens observed per edge):"
                                ++ concatMap
                                  (\(ename, sz) -> "\n  " ++ show ename ++ ": " ++ show sz)
                                  internalBufSizes

                            ioBufStr =
                              "\n\nI/O buffer sizes (rate × repetition count):"
                                ++ concatMap
                                  (\(ename, sz) -> "\n  " ++ show ename ++ ": " ++ show sz)
                                  (fst ioBufSizes)

                            allBufStr =
                              "\n\nAll buffer sizes (I/O + internal):"
                                ++ concatMap
                                  (\(ename, sz) -> "\n  " ++ show ename ++ ": " ++ show sz)
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
       in outputString ++ "\n"

-- Helper function to display the null space vector, one component per line
nullspaceVectorToString :: [Rational] -> String
nullspaceVectorToString = unlines . map formatRational
  where
    formatRational x
      | denominator x == 1 = show (numerator x)
      | otherwise = show (numerator x) ++ "/" ++ show (denominator x)
