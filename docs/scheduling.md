# Scheduling Documentation
A simple implementation of SDF Graph Scheduling with basic verification

## Overview
This tool takes a ForSyDeIR 'IRSystem' and generates:
- A valid execution schedule that respects data dependencies
- The required buffer size for each edge during execution

The function can be used is defined as follows:
```Haskell
-- Returns schedule as actor names and buffer sizes
computeScheduleAndBuffers :: IRSystem -> ([String], [Int])
-- Prints topology matrix, repetition vector, schedule, verification results, and buffer usage.
computeScheduleAndBuffersPrint :: IRSystem -> IO ()
```

## Preprocessing
Before generating the schedule, the IRSystem is converted to a data structure for better usage in scheduler.
The scheduler data structure is defined as:
```Haskell
data Actor = Actor { name :: String
                   , isInput :: Bool 
                   } deriving (Show, Eq, Generic)

data Edge  = Edge  { src :: Actor
                   , dst :: Actor
                   , prod :: Int
                   , cons :: Int 
                   , isDelay :: Bool
                   , initTokens :: Int  -- Count of Init tokens for delay edges
                   } deriving (Show, Eq, Generic)
```
The proprocessing steps are:
1. **Input/Output Signal Matching**
    - Find actors that receive signals from source named `"input"` and flagged as `isInput`
    - Currently, it can only match one input named `"input"`
    - `"output"` is discarded

2. **Delay Actor Folding**
    - Delay actors in the IR (e.g., `IRDelay`) are folded into edges
    - For a delay between Actor A → Delay → Actor B, a single edge from A → B is created
    - The count of initial tokens on these folded edges are preserved for scheduling

3. **Self-loop Validation**
    - Edges where the source and destination are the same actor (self-loops) are checked
    - If the production rate (`prod`) and consumption rate (`cons`) differ, the graph is considered invalid
    - Legal self-loops (prod == cons) are retained but do not contribute to the topology matrix

## Core Algorithm
The algorithm works in three phases:
1. **Topology Matrix and Repetition Vector Generation**
    - Generate the Topology Matrix from the input actors and edges
      - Self-loops are treated as zero rows in the matrix 
    - Calculate nullspace of the matrix and approximate to minimum integer
    - Verify the approximated repetition vector

2. **Schedule Generation and Initial Token Calculation**
    - Choose the first fireable actor to fire
    - When no actor is fireable, deadlock happens and becomes error
    - Record the execution order

3. **Buffer Analysis**
    - Simulate the schedule execution
    - Track maximum token usage for each edge
    - Return the maximum buffer size required

## Usage Example

You can use the scheduler either as a library function to get schedule and buffer sizes, or use the pretty-print version to see detailed intermediate results.

```haskell
main :: IO ()
main = do
  -- Using the library function to get schedule (as actor names) and buffer sizes
  let (schedNames, bufSizes) = computeScheduleAndBuffersNames exampleSystem
  putStrLn "Schedule and buffer sizes (library function):"
  putStrLn $ "Schedule: " ++ show schedNames
  putStrLn $ "Buffer sizes: " ++ show bufSizes

  -- Using the pretty-print version to see detailed info
  putStrLn "\n--- Pretty-print version ---\n"
  computeScheduleAndBuffersPrint exampleSystem
```

## Limitation and Future Work
Currently, this scheduler will generate a valid schedule, but not with a minimal buffer size. This could be optimized with new strategy of choosing fireable actors.

Besides, the identify actors that connects to an input, currently it simply finds an IRSignal that source is named "input". If there are multiple inputs, the name might be different and this would not work. There should be a new way to represent inputs in ForSyDeIR.