module ForSyDeIR where

import GHC.Core
import GHC.Core.Ppr (pprCoreExpr)
import GHC.Utils.Outputable

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
  = IRDelay String
  | IRActor String ActorType String

data IRSignal = IRSignal String (String, Int) (String, Int)

data IRFunction = IRFunction String (Maybe CoreExpr)

data IRSystem = IRSystem [IRConstructor] [IRSignal] [IRFunction]

-- Pretty printing functions for ForSyDe IR

prettyIRSignal :: IRSignal -> SDoc
prettyIRSignal (IRSignal signalId (inputId, inputRate) (outputId, outputRate)) =
  text "IRSignal"
    <+> parens
      ( text signalId
          <+> comma
          <+> parens
            ( text inputId
                <+> comma
                <+> int inputRate
            )
          <+> comma
          <+> parens
            ( text outputId
                <+> comma
                <+> int outputRate
            )
      )

prettyIRConstructor :: IRConstructor -> SDoc
prettyIRConstructor (IRDelay delayId) =
  text "IRDelay"
    <+> parens
      (text delayId)
prettyIRConstructor (IRActor actorId actorType functionId) =
  text "IRActor"
    <+> parens
      ( text actorId
          <+> comma
          <+> text (show actorType)
          <+> comma
          <+> text functionId
      )

prettyIRFunction :: IRFunction -> SDoc
prettyIRFunction (IRFunction functionId function) =
  text "IRFunction"
    <+> parens
      ( text functionId
          <+> comma
          <+> maybe empty pprCoreExpr function
      )

prettyIRSystem :: IRSystem -> SDoc
prettyIRSystem (IRSystem constructors signals functions) =
  text "IRSystem"
    <+> parens
      ( vcat
          [ text "{"
              $$ nest
                2
                ( vcat (punctuate comma (map prettyIRConstructor constructors))
                )
              $$ text "}",
            text "{"
              $$ nest
                2
                ( vcat (punctuate comma (map prettyIRSignal signals))
                )
              $$ text "}",
            text "{"
              $$ nest
                2
                ( vcat (punctuate comma (map prettyIRFunction functions))
                )
              $$ text "}"
          ]
      )

-- Simple example to test ForSyDe IR

exampleSystem :: IRSystem
exampleSystem =
  IRSystem
    [ IRActor "actor_1" Actor22 "add",
      IRDelay "delay_1"
    ]
    [ IRSignal "s_in" ("input", 1) ("actor_1", 1),
      IRSignal "s_1" ("actor_1", 1) ("delay_1", 1),
      IRSignal "s_2" ("delay_1", 1) ("actor_1", 1),
      IRSignal "s_out" ("actor_1", 1) ("output", 1)
    ]
    [ IRFunction "add" Nothing
    ]

testForSyDeIR :: IO ()
testForSyDeIR = do
  putStrLn $ showSDocUnsafe $ prettyIRSystem exampleSystem
