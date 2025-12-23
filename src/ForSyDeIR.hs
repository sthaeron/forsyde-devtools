{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
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
    IRId (..),
    IRSpan,
    varToSpan,
  )
where

import CoreIR (prettyCoreExpr, varToString)
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
import qualified GHC.Data.FastString as FS
import GHC.Plugins (Var, nameSrcSpan, varName, varUnique)
import GHC.Types.SrcLoc
import Text.Printf (printf)

-- ForSyDe IR data types

data IRId
  = IRVar Var
  | IRString String
  | Empty

instance Show IRId where
  show :: IRId -> String
  show (IRVar i) = varToString i
  show (IRString s) = s
  show Empty = ""

instance Eq IRId where
  (==) (IRVar a) (IRVar b) = varUnique a == varUnique b
  (==) (IRString a) (IRString b) = a == b
  -- NOTE: This implies Empty is not equal to empty
  (==) Empty _ = False
  (==) _ Empty = False
  (==) (IRString a) b = a == show b
  (==) a (IRString b) = show a == b

prettyIRId :: IRId -> String
prettyIRId = show . show

type IRSpan = (String, Int, Int, Int, Int)

varToSpan :: IRId -> Maybe IRSpan
varToSpan v =
  irVarToVar v
    >>= varToRealSrcSpan
    >>= \l ->
      Just
        ( FS.unpackFS $ srcSpanFile l,
          srcSpanStartLine l,
          srcSpanStartCol l,
          srcSpanEndLine l,
          srcSpanEndCol l
        )
  where
    irVarToVar = \case
      IRVar i -> Just i
      _ -> Nothing
    varToRealSrcSpan i =
      case nameSrcSpan $ varName i of
        RealSrcSpan loc _ -> Just loc
        _ -> Nothing

prettyIRSpan :: IRSpan -> String
prettyIRSpan (file, startline, startcol, stopline, stopcol)
  | startline == stopline && startcol == stopcol =
      printf "IRSpan(\"%s\", %d:%d)" file startline startcol
  | startline == stopline =
      printf "IRSpan(\"%s\", %d:(%d-%d))" file startline startcol stopcol
  | startcol == stopcol =
      printf "IRSpan(\"%s\", (%d-%d):%d)" file startline stopline stopcol
  | otherwise =
      printf "IRSpan(\"%s\", (%d-%d):(%d-%d))" file startline stopline startcol stopcol

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
-- IRActor(actorId, actorType, functionId (inputSignals, outputSignals))
data IRConstructor
  = IRDelay IRId [Int] (IRId, IRId)
  | IRActor IRId ActorType IRId ([IRId], [IRId])

-- IRSignal(signalId (sourceId, sourceRate) (targetId, targetRate))
data IRSignal = IRSignal IRId (IRId, Int) (IRId, Int)

-- IRFunction(functionId, maybe function)
data IRFunction = IRFunction IRId (Maybe CoreExpr)

-- IRSystem((globalInputs, globalOutputs), constructors, signals, functions)
data IRSystem = IRSystem ([IRId], [IRId]) [IRConstructor] [IRSignal] [IRFunction]

-- ForSyDe IR pretty printing functions

indent :: Int -> String -> String
indent numberSpaces = unlines . map (replicate numberSpaces ' ' ++) . lines

prettyIRSignal :: IRSignal -> String
prettyIRSignal (IRSignal signalId (inputId, inputRate) (outputId, outputRate)) =
  printf
    "IRSignal(\"%s\", (\"%s\", %d), (\"%s\", %d), %s)"
    (show signalId)
    (show inputId)
    inputRate
    (show outputId)
    outputRate
    (maybe "" prettyIRSpan $ varToSpan signalId)

prettyIRConstructor :: IRConstructor -> String
prettyIRConstructor (IRDelay delayId tokens (input, output)) =
  printf
    "IRDelay(\"%s\", {%s}, %s, %s, %s)"
    (show delayId)
    (intercalate ", " (map show tokens))
    (prettyIRId input)
    (prettyIRId output)
    (maybe "" prettyIRSpan $ varToSpan delayId)
prettyIRConstructor (IRActor actorId actorType functionId (inputs, outputs)) =
  printf
    "IRActor(\"%s\", %s, \"%s\", {%s}, {%s}, %s)"
    (show actorId)
    (show actorType)
    (show functionId)
    (intercalate ", " (map prettyIRId inputs))
    (intercalate ", " (map prettyIRId outputs))
    (maybe "" prettyIRSpan $ varToSpan actorId)

prettyIRFunction :: DynFlags -> IRFunction -> String
prettyIRFunction dflags (IRFunction functionId function) =
  printf
    "IRFunction(\"%s\", %s, %s)"
    (show functionId)
    (maybe "" (prettyFunction dflags) function)
    (maybe "" prettyIRSpan $ varToSpan functionId)

prettyFunction :: DynFlags -> CoreExpr -> String
prettyFunction dflags function = printf "\n%s" (indent 2 (prettyCoreExpr dflags function))

prettyIRSystem :: DynFlags -> IRSystem -> String
prettyIRSystem dflags (IRSystem (inputs, outputs) constructors signals functions) =
  printf
    "IRSystem(\n  {%s}, {%s},\n  {\n%s  },\n  {\n%s  },\n  {\n%s  }\n)\n"
    (intercalate ", " (map prettyIRId inputs))
    (intercalate ", " (map prettyIRId outputs))
    (indent 4 (intercalate ",\n" (map prettyIRConstructor constructors)))
    (indent 4 (intercalate ",\n" (map prettyIRSignal signals)))
    (indent 4 (intercalate ",\n" (map (prettyIRFunction dflags) functions)))

instance Show IRSystem where
  show (IRSystem (inputs, outputs) constructors signals _) =
    printf
      "IRSystem(\n  {%s}, {%s},\n  {\n%s  },\n  {\n%s  },\n  {}\n)\n"
      (intercalate ", " (map prettyIRId inputs))
      (intercalate ", " (map prettyIRId outputs))
      (indent 4 (intercalate ",\n" (map prettyIRConstructor constructors)))
      (indent 4 (intercalate ",\n" (map prettyIRSignal signals)))

-- ForSyDe IR to JSON functions

instance ToJSON ActorType where
  toJSON a = String $ Text.pack $ show a

instance ToJSON IRConstructor where
  toJSON (IRDelay name tokens (_, _)) =
    object
      [ "type" .= Text.pack "Delay",
        "name" .= Text.pack (show name),
        "tokens" .= Seq.fromList tokens
      ]
  toJSON (IRActor name ty func (_, _)) =
    object
      [ "type" .= Text.pack (show ty),
        "name" .= Text.pack (show name),
        "function" .= Text.pack (show func)
      ]

instance ToJSON IRSignal where
  toJSON (IRSignal name (source, sourceRate) (target, targetRate)) =
    object
      [ "name" .= Text.pack (show name),
        "source"
          .= object
            [ "name" .= Text.pack (show source),
              "rate" .= sourceRate
            ],
        "target"
          .= object
            [ "name" .= Text.pack (show target),
              "rate" .= targetRate
            ]
      ]

instance ToJSON IRFunction where
  toJSON (IRFunction name _) =
    object
      [ "name" .= Text.pack (show name)
      -- "coreexpr" .= ...
      ]

instance ToJSON IRSystem where
  toJSON (IRSystem (inputs, outputs) processes signals functions) =
    object
      [ "system"
          .= object
            [ "inputs" .= Seq.fromList (map Text.show inputs),
              "outputs" .= Seq.fromList (map Text.show outputs),
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
