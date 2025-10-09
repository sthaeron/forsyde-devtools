# Scheduling Documentation
A simple implementation of SDF Graph Scheduling with basic verification

## Overview
This tool takes a ForSyDe SDF Graph (actors and edges) and generates:
- A valid execution schedule that respects data dependencies
- The minimum initial tokens required to avoid deadlock
- The required buffer size for each edge during execution

## Core Algorithm
The algorithm works in three phases:
1. **Topology Matrix and Repetition Vector Generation**
    - Generate the Topology Matrix from the input actors and edges
    - Calculate nullspace of the matrix and approximate to minimum integer
    - Verify the approximated repetition vector
2. **Schedule Generation and Initial Token Calculation**
    - Choose the first fireable actor to fire
    - When no actor is fireable, force fire the first one with remaining repetition (breaking deadlocks)
    - Records the execution order
    - Tracks the minimum token count for each edge during execution
    - Converts negative minimums to positive initial tokens
3. **Buffer Analysis**
    - Simulates the schedule execution
    - Tracks maximum token usage for each edge
    - Returns the maximum buffer size required

## Limitation and Future Work
Currently, this scheduler will generate a valid schedule, but not with a minimal buffer size. This could be optimized with new strategy of choosing fireable actors.

In addition, when there are no fireable actors due to cycles in the graph, it will force fire the first actor with remaing repetition, and update its initial token count. For example, in SDF_example, there is an edge from C to D, and a delay edge from D to C. An initial token could be place on both edge to start a valid schedule, but the meaning and the functionality of the graph might differ. This should be check with Ingo.