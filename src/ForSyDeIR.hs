{-# LANGUAGE OverloadedStrings #-}

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
    prettyIRJSON,
  )
where

import CoreIR (prettyCoreExpr)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Function
import Data.List (intercalate)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
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

-- IRDelay(delayId, tokens, (inputSignal, outputSignal))
-- IRActor(actorId, actorType, (inputSignals, outputSignals))
data IRConstructor
  = IRDelay String [Int] (String, String)
  | IRActor String ActorType String ([String], [String])

-- IRSignal(signalId (sourceId, sourceRate) (targetId, targetRate))
data IRSignal = IRSignal String (String, Int) (String, Int)

-- IRFunction(functionId, maybe function)
data IRFunction = IRFunction String (Maybe CoreExpr)

-- IRSystem((globalInputs, globalOutputs), constructors, signals, functions)
data IRSystem = IRSystem ([String], [String]) [IRConstructor] [IRSignal] [IRFunction]

-- ForSyDe IR pretty printing functions

indent :: Int -> String -> String
indent numberSpaces = unlines . map (replicate numberSpaces ' ' ++) . lines

prettyIRSignal :: IRSignal -> String
prettyIRSignal (IRSignal signalId (inputId, inputRate) (outputId, outputRate)) =
  printf "IRSignal(\"%s\", (\"%s\", %d), (\"%s\", %d))" signalId inputId inputRate outputId outputRate

prettyIRConstructor :: IRConstructor -> String
prettyIRConstructor (IRDelay delayId tokens (input, output)) =
  printf
    "IRDelay(\"%s\", {%s}, %s, %s)"
    delayId
    (intercalate ", " (map show tokens))
    (show input)
    (show output)
prettyIRConstructor (IRActor actorId actorType functionId (inputs, outputs)) =
  printf
    "IRActor(\"%s\", %s, \"%s\", {%s}, {%s})"
    actorId
    (show actorType)
    functionId
    (intercalate ", " (map show inputs))
    (intercalate ", " (map show outputs))

prettyIRFunction :: DynFlags -> IRFunction -> String
prettyIRFunction dflags (IRFunction functionId function) =
  printf
    "IRFunction(\"%s\", %s)"
    functionId
    (maybe "" (prettyFunction dflags) function)

prettyFunction :: DynFlags -> CoreExpr -> String
prettyFunction dflags function = printf "\n%s" (indent 2 (prettyCoreExpr dflags function))

prettyIRSystem :: DynFlags -> IRSystem -> String
prettyIRSystem dflags (IRSystem (inputs, outputs) constructors signals functions) =
  printf
    "IRSystem(\n  {%s}, {%s},\n  {\n%s  },\n  {\n%s  },\n  {\n%s  }\n)\n"
    (intercalate ", " (map show inputs))
    (intercalate ", " (map show outputs))
    (indent 4 (intercalate ",\n" (map prettyIRConstructor constructors)))
    (indent 4 (intercalate ",\n" (map prettyIRSignal signals)))
    (indent 4 (intercalate ",\n" (map (prettyIRFunction dflags) functions)))

-- ForSyDe IR to JSON functions

instance ToJSON ActorType where
  toJSON a = String $ Text.pack $ show a

instance ToJSON IRConstructor where
  toJSON (IRDelay name tokens (_, _)) =
    object
      [ "type" .= Text.pack "Delay",
        "name" .= Text.pack name,
        "tokens" .= Seq.fromList tokens
      ]
  toJSON (IRActor name ty func (_, _)) =
    object
      [ "type" .= Text.pack (show ty),
        "name" .= Text.pack name,
        "function" .= Text.pack func
      ]

instance ToJSON IRSignal where
  toJSON (IRSignal name (source, sourceRate) (target, targetRate)) =
    object
      [ "name" .= Text.pack name,
        "source"
          .= object
            [ "name" .= Text.pack source,
              "rate" .= sourceRate
            ],
        "target"
          .= object
            [ "name" .= Text.pack target,
              "rate" .= targetRate
            ]
      ]

instance ToJSON IRFunction where
  toJSON (IRFunction name _) =
    object
      [ "name" .= Text.pack name
      -- "coreexpr" .= ...
      ]

instance ToJSON IRSystem where
  toJSON (IRSystem (inputs, outputs) processes signals functions) =
    object
      [ "system"
          .= object
            [ "inputs" .= Seq.fromList inputs,
              "outputs" .= Seq.fromList outputs,
              "processes" .= Seq.fromList processes,
              "signals" .= Seq.fromList signals,
              "functions" .= Seq.fromList functions
            ]
      ]

prettyIRJSON :: (ToJSON a) => a -> String
prettyIRJSON v =
  TLB.singleton '\n'
    & mappend (encodePrettyToTextBuilder v)
    & TLB.toLazyText
    & TL.unpack
