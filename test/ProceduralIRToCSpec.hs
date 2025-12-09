module ProceduralIRToCSpec (spec) where

import ArgumentsMain
import Data.Char (isSpace)
import ProceduralIR
import ProceduralIRToC
import Test.Hspec

exampleProceduralIR :: Program
exampleProceduralIR =
  Prog
    [ GFuncDef
        Nothing
        TInt
        "main"
        []
        ( SScope
            [ SVarDecl (TIdent "token") "input",
              SVarDecl (TIdent "token") "output",
              SVarDecl TInt "i",
              SVarDecl TInt "j",
              SVarDef
                (TPointer (TIdent "channel"))
                "s_in"
                ( ECall
                    "create_buffer_nonblocking"
                    ([EInt 4])
                ),
              SVarDef
                (TPointer (TIdent "channel"))
                "s_out"
                ( ECall
                    "create_buffer_nonblocking"
                    ([EInt 2])
                ),
              SVarDef
                (TPointer (TIdent "channel"))
                "s_1"
                ( ECall
                    "create_buffer_nonblocking"
                    ([EInt 1])
                ),
              SVarDef
                (TPointer (TIdent "channel"))
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
                (EBinOp Less (EVar "i") (EVar "n"))
                ((SScope [(SReturn Nothing)]))
                Nothing,
              SIf
                (EBinOp Greater (EVar "i") (EInt 0))
                ((SScope [SReturn (Just (EInt 1))]))
                (Just (SScope [(SReturn (Just (EInt 0)))])),
              SReturn (Just (EInt 0)),
              SReturn Nothing,
              SFor
                (SVarDef TInt "i" (EInt 0))
                (EBinOp Less (EVar "i") (EVar "n"))
                (SExpr (EUnOp Increment (EVar "i")))
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
        (Just Static)
        TVoid
        "actor11SDF"
        [ (TInt, "consum"),
          (TInt, "prod"),
          (TPointer (TIdent "channel"), "ch_in"),
          (TPointer (TIdent "channel"), "ch_out"),
          ( TFunctionPointer
              TVoid
              [ (TPointer (TIdent "token")),
                (TPointer (TIdent "token"))
              ],
            "f"
          )
        ]
        (SScope [])
    ]

spec :: Spec
spec = beforeAll readExpectedCode $ do
  describe "Procedural IR To C Codegen" $ do
    it "Test hand-crafted Procedural IR" $ \simpleCString -> do
      let cString = translateProgram exampleProceduralIR PC StdIn False
      normalize cString `shouldBe` normalize simpleCString
  where
    normalize = filter (not . isSpace)
    readExpectedCode = readFile "examples/test/simple.c"
