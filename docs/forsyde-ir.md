# ForSyDe IR Documentation
The ForSyDe IR can be found in the file `src/ForSyDeIR.hs`. The defined data types for the IR in Haskell are provided below.
```haskell
data ActorType
  = Actor11
  | Actor22
data IRConstructor 
  = IRDelay String
  | IRActor String ActorType String
data IRSignal = IRSignal String (String, Int) (String, Int)
data IRFunction = IRFunction String (Maybe CoreExpr)
data IRSystem = IRSystem [IRConstructor] [IRSignal] [IRFunction]
```

## Pretty Printing
The pretty printing for the ForSyDe IR is also defined under `src/ForSyDeIR.hs`. A summary is provided below. Note that whitespace should be ignored. The pretty printing for `ActorType` is the same as the type it represents.
- `constructor`: `IRDelay(delayId)`, `IRActor(actorID, actorType, functionId)`
- `signal`: `IRSignal(signalId, (sourceId, sourceRate), (targetId, targetRate))`
- `function`: `IRFunction(functionId, [ function ])`
- `system`: `IRSystem({ constructors }, { signals }, { functions } )`

## Naming Convention
The naming convention of the ForSyDe IR data types is to just use the umbrella term provided by the ForSyDe Shallow documentation and prefix `IR`. Make sure to use upper camel case.
