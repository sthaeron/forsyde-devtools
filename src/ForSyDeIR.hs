module ForSyDeIR
  ( ActorType (..),
    IRConstructor (..),
    IRSignal (..),
    IRFunction (..),
    IRSystem (..),
    prettyIRSignal,
    prettyIRConstructor,
    prettyIRFunction,
    prettyIRSystem,
  )
where

import CoreIR (prettyCoreExpr)
import Data.List (intercalate)
import GHC (DynFlags)
import GHC.Core
import Text.Printf (printf)

-- ForSyDe IR data types

data ActorType
  = Actor11
  | Actor12
  | Actor13
  | Actor14
  | Actor21
  | Actor22
  | Actor23
  | Actor24
  | Actor31
  | Actor32
  | Actor33
  | Actor34
  | Actor41
  | Actor42
  | Actor43
  | Actor44
  deriving (Show)

data IRConstructor
  = IRDelay String [Int]
  | IRActor String ActorType String -- ([String], [String])

data IRSignal = IRSignal String (String, Int) (String, Int) -- IRSignal(signalName (input, sourceRate) (targetId, targetRate))

data IRFunction = IRFunction String (Maybe CoreExpr)

data IRSystem = IRSystem ([String], [String]) [IRConstructor] [IRSignal] [IRFunction]

indent :: String -> String
indent = unlines . map ("    " ++) . lines

prettyIRSignal :: IRSignal -> String
prettyIRSignal (IRSignal signalId (inputId, inputRate) (outputId, outputRate)) =
  printf "IRSignal(\"%s\", (\"%s\", %d), (\"%s\", %d))" signalId inputId inputRate outputId outputRate

prettyIRConstructor :: IRConstructor -> String
prettyIRConstructor (IRDelay delayId tokenList) =
  printf "IRDelay(\"%s\", {%s})" delayId (intercalate ", " (map show tokenList))
prettyIRConstructor (IRActor actorId actorType functionId) =
  printf "IRActor(\"%s\", %s, \"%s\")" actorId (show actorType) functionId

prettyIRFunction :: DynFlags -> IRFunction -> String
prettyIRFunction dflags (IRFunction functionId function) =
  printf "IRFunction(\"%s\", %s)" functionId (maybe "" (prettyCoreExpr dflags) function)

prettyIRSystem :: DynFlags -> IRSystem -> String
prettyIRSystem dflags (IRSystem (inputs, outputs) constructors signals functions) =
  printf
    "IRSystem(\n  {%s}, {%s},\n  {\n%s  },\n  {\n%s  },\n  {\n%s  }\n)"
    (intercalate ", " (map show inputs))
    (intercalate ", " (map show outputs))
    (indent (intercalate ",\n" (map prettyIRConstructor constructors)))
    (indent (intercalate ",\n" (map prettyIRSignal signals)))
    (indent (intercalate ",\n" (map (prettyIRFunction dflags) functions)))

