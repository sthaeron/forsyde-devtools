# Scheduling Documentation
A simple implementation of SDF Graph Scheduling with basic verification

## Overview
This tool takes a ForSyDeIR 'IRSystem' and generates:
- A valid execution schedule that respects data dependencies
- The required buffer size for each edge during execution

The function can be used is defined as follows:
```Haskell
-- | Returns schedule as actor names, buffer sizes, and delay buffer mappings
-- Returns: (schedule_order, [(buffer_name, buffer_size)], [(original_signal, delay_buffer_name)])
computeScheduleAndBuffers :: IRSystem -> ([IRId], [(IRId, Int)], [(IRId, IRId)])

-- | Returns a string with topology matrix, repetition vector, schedule, verification results, and buffer usage.
computeScheduleAndBuffersPrint :: IRSystem -> String
```

## Preprocessing
Before generating the schedule, the IRSystem is converted to a data structure for better usage in scheduler.
The scheduler data structure is defined as:
```Haskell
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
```
The proprocessing steps are:
1. **Actor and Edge Construction**
    - Create actors for all `IRActor` nodes (excluding delays)
    - Mark actors as input if they receive signals from global inputs or are global inputs themselves
    - Create edges for signals that are not connected to delays, global inputs, or global outputs

2. **Delay Actor Folding**
    - Identify all `IRDelay` nodes in the constructors
    - Delay actors are folded into edges
    - For a delay between Actor A → Delay → Actor B, a single edge from A → B is created. Input signal id is used as the delay edge id.
    - Create a mapping of original signals and delay buffers.
    - Ignore delays connected to global I/O - if delay input comes from global input or output goes to global output, the delay edge is omitted.
    - Preserve initial token count from the delay constructor.

3. **Self-loop Validation**
    - Edges where the source and destination are the same actor (self-loops) are checked
    - If the production rate (`prod`) and consumption rate (`cons`) differ, the graph is considered invalid
    - Legal self-loops (prod == cons) are retained but do not contribute to the topology matrix

## Core Algorithm
The algorithm works in three phases:
1. **Topology Matrix and Repetition Vector Generation**
    - Generate the Topology Matrix from the input actors a nd edges
      - Self-loops are treated as zero rows in the matrix (do not affect the matrix)
    - Calculate nullspace of the matrix and approximate to minimum integer
    - Verify the approximated repetition vector

2. **Schedule Generation**
    - Choose the first fireable actor to fire
    - When no actor is fireable, deadlock happens and becomes error
    - Record the execution order

3. **Buffer Analysis**
    - **Internal Buffer Simulation**:
        1. Simulate the schedule execution with initial tokens.
        2. For each edge, track the maximum number of tokens present during simulation.
        3. This maximum becomes the required buffer size for that edge.

    - **I/O Buffer Calculation**:
        1. **Input buffers**: `consumptionRate × destinationActorRepetitionCount`.
        2. **Output buffers**: `productionRate × sourceActorRepetitionCount`.
        3. These are calculated separately for edges connected to global inputs/outputs.

    - **Combined Results**: Internal and I/O buffer sizes are concatenated in the final output.
   
    - **Delay Buffer Mapping**: For delay edges, maintain mapping between original signal IDs and their corresponding buffer names.

### Special Case:
If the system has no internal edges (only I/O connections):
- All actors fire exactly once in arbitrary order.
- No internal buffers required (all buffer sizes = 0).
- I/O buffers are still calculated based on rates.

## Usage Example

You can use the scheduler either as a library function to get schedule and buffer sizes, or use the pretty-print version to see detailed intermediate results.

```haskell
main :: IO ()
main = do
  -- Using the library function to get schedule, buffer sizes, and delay buffer mapping
  let (schedule, buffers, delayBuffers) = computeScheduleAndBuffers forsydeIR
```

## Limitation and Future Work
- Currently the scheduler cannot convert an IRSystem with delays that is connected together. It uses `findActorByName` on the `src` and `dst` that a delay actor connects to, which should find the name of non-delay actor, where an error would occur if it is a delay actor. Updating the function to recursively find the first non-delay actor would fix the issue.
- The scheduler generates a valid schedule but not necessarily with minimal buffer size. Buffer size optimization could be improved with better actor selection strategies.
- The scheduler uses Hmatrix which depends on C libraries. It would be better to use a pure Haskell matrix library so that the executable would be easier to link as well as being able to build with cabal without installing external dependencies.