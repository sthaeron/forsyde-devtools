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

data IRConstructor
  = IRDelay String
  | IRActor String ActorType String

data IRSignal = IRSignal String (String, Int) (String, Int)

data IRFunction = IRFunction String (Maybe CoreExpr)

data IRSystem = IRSystem [IRConstructor] [IRSignal] [IRFunction]

-- Pretty printing functions for ForSyDe IR

prettyActorType :: ActorType -> SDoc
prettyActorType Actor11 = text "Actor11"
prettyActorType Actor12 = text "Actor12"
prettyActorType Actor13 = text "Actor13"
prettyActorType Actor14 = text "Actor14"
prettyActorType Actor21 = text "Actor21"
prettyActorType Actor22 = text "Actor22"
prettyActorType Actor23 = text "Actor23"
prettyActorType Actor24 = text "Actor24"
prettyActorType Actor31 = text "Actor31"
prettyActorType Actor32 = text "Actor32"
prettyActorType Actor33 = text "Actor33"
prettyActorType Actor34 = text "Actor34"
prettyActorType Actor41 = text "Actor41"
prettyActorType Actor42 = text "Actor42"
prettyActorType Actor43 = text "Actor43"
prettyActorType Actor44 = text "Actor44"

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
          <+> prettyActorType actorType
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
