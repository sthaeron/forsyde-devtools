module ProceduralIRSpec (spec) where

import Data.Char (isSpace)
import ProceduralIR
import Test.Hspec

exampleProceduralIR :: Program
exampleProceduralIR =
  Prog
    [ GFuncDef
        []
        TInt
        "main"
        []
        ( SScope
            [ SVarDecl (TIdent "token") "input",
              SVarDecl (TIdent "token") "output",
              SVarDecl TInt "i",
              SVarDecl TInt "j",
              SVarDef
                (TPoint (TIdent "channel"))
                "s_in"
                ( ECall
                    "create_buffer_nonblocking"
                    ([EInt 4])
                ),
              SVarDef
                (TPoint (TIdent "channel"))
                "s_out"
                ( ECall
                    "create_buffer_nonblocking"
                    ([EInt 2])
                ),
              SVarDef
                (TPoint (TIdent "channel"))
                "s_1"
                ( ECall
                    "create_buffer_nonblocking"
                    ([EInt 1])
                ),
              SVarDef
                (TPoint (TIdent "channel"))
                "s_1_delay"
                ( ECall
                    "create_buffer_nonblocking"
                    ([EInt 1])
                ),
              SExpr
                ( ECall
                    "writeToken"
                    ( [ EVar "s_1",
                        EInt 0
                      ]
                    )
                ),
              SWhile
                (EInt 1)
                ( SScope
                    [ SExpr
                        ( ECall
                            "actor11SDF"
                            [ EInt 2,
                              EInt 1,
                              EVar "s_in",
                              EVar "s_1",
                              EVar "f_1"
                            ]
                        )
                    ]
                ),
              SArrayAssign "x" (EInt 0) Nothing (EInt 3),
              SArrayAssign "x" (EInt 1) (Just "foo") (EInt 2),
              SIf
                (EBinOp OPLess (EVar "i") (EVar "n"))
                (SReturn Nothing)
                Nothing,
              SIf
                (EBinOp OPGreater (EVar "i") (EInt 0))
                (SReturn (Just (EInt 1)))
                (Just (SReturn (Just (EInt 0)))),
              SReturn (Just (EInt 0)),
              SReturn Nothing,
              SFor
                (SVarDef TInt "i" (EInt 0))
                (EBinOp OPLess (EVar "i") (EVar "n"))
                (SExpr (EUnOp OPIncrement (EVar "i")))
                (SScope [SExpr (ECall "printf" [EString "%d\n", EVar "i"])]),
              SVarDef
                (TReference (TIdent "fifo"))
                "fifo"
                (EVar "f"),
              SExpr (ECallExpr (EVar "printf") [EString "Hi"]),
              SExpr (ECallExpr (EPointerAccess (EVar "a") "funca") []),
              SExpr
                ( ECallExpr
                    ( EMemberAccess
                        (ECallExpr (EMemberAccess (EVar "a") "getB") [])
                        "printB"
                    )
                    []
                ),
              SExpr
                ( EPointerAccess
                    (EArrayAccess (EVar "p") (EVar "i"))
                    "value"
                ),
              SExpr
                ( ECall
                    "pthread_mutex_lock"
                    [ EReference
                        (EPointerAccess (EVar "fifo") "lock")
                    ]
                ),
              SVarDef
                TInt
                "value"
                (EDereference (EVar "p")),
              SAssign
                (EDereference (EVar "p"))
                (EInt 10),
              SArrayDecl
                TInt
                "input"
                [EInt 2]
            ]
        ),
      GFuncDef
        ["static"]
        TVoid
        "actor11SDF"
        [ (TInt, "consum"),
          (TInt, "prod"),
          (TPoint (TIdent "channel"), "ch_in"),
          (TPoint (TIdent "channel"), "ch_out"),
          ( TFuncPoint
              TVoid
              [ (TPoint (TIdent "token")),
                (TPoint (TIdent "token"))
              ],
            "f"
          )
        ]
        (SScope [])
    ]

spec :: SpecWith ()
spec = do
  describe "Procedural IR pretty-printing" $ do
    it "Test hand-crafted Procedural IR" $ do
      simplePIRString <- readFile "examples/test/simple.pir"
      normalize (prettyProgram exampleProceduralIR)
        `shouldBe` normalize (simplePIRString)
  where
    normalize = filter (not . isSpace)
