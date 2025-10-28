module ProceduralIR where

import GHC.Utils.Outputable
import Prelude hiding ((<>))

data Uop
  = OPNegate -- -rhs
  | OPLogicalNot -- !rhs
  | OPIncrement -- ++rhs
  | OPDecrement -- --rhs

data Bop
  = OPPlus -- lhs + rhs
  | OPMinus -- lhs - rhs
  | OPMultiply -- lhs * rhs
  | OPDivide -- lhs / rhs
  | OPModulo -- lhs % rhs
  | OPPlusAssign -- lhs += rhs
  | OPMinusAssign -- lhs -= rhs
  | OPMultiplyAssign -- lhs *= rhs
  | OPDivideAssign -- lhs /= rhs
  | OPModuloAssign -- lhs %= rhs
  | OPEqual -- lhs == rhs
  | OPNotEqual -- lhs != rhs
  | OPLogicalAnd -- lhs && rhs
  | OPLogicalOr -- lhs || rhs
  | OPLess -- lhs < rhs
  | OPLessEqual -- lhs <= rhs
  | OPGreater -- lhs > rhs
  | OPGreaterEqual -- lhs >= rhs

-- Types
data Type
  = TVoid
  | TInt
  | TFloat
  | TChar
  | TIdent String
  | TPoint Type -- int * var
  | TReference Type -- int & var
  | TFuncPoint Type [Type] -- void (*func)([type])

-- Expressions
data Expression
  = EVar String
  | EInt Int
  | EChar Char
  | EString String
  | EBinOp Bop Expression Expression
  | EUnOp Uop Expression
  | ECall String [Expression] -- string(), simple function call
  | ECallExpr Expression [Expression] -- abc.foo(), enable more complex function call
  | EArrayAccess Expression Expression -- expr[expr], modified from cigrid for better expressiveness
  | EReference Expression -- &expr
  | EDereference Expression
  | -- \*expr
    EMemberAccess Expression String -- expr.string
  | EPointerAccess Expression String -- expr->string
  | EParen Expression -- (expr)

-- Statements
data Statement
  = SExpr Expression
  | SVarDecl Type String -- Token input; int i;
  | SVarDef Type String Expression
  | SAssign Expression Expression -- expr = expr;
  | SVarAssign String Expression
  | SArrayDecl Type String [Expression] -- int output[2];
  | SArrayAssign String Expression (Maybe String) Expression
  | SScope [Statement]
  | SIf Expression Statement (Maybe Statement)
  | SWhile Expression Statement
  | SFor Statement Expression Statement Statement
  | SBreak
  | SReturn (Maybe Expression)
  | SGoto String
  | SLabel String

-- Globals
data Global
  = GFuncDef [String] Type String [(Type, String)] Statement

data Program = Prog [Global]

-- Pretty printing functions for ProceduralIR
prettyUop :: Uop -> SDoc
prettyUop OPNegate = text "-"
prettyUop OPLogicalNot = text "!"
prettyUop OPIncrement = text "++"
prettyUop OPDecrement = text "--"

prettyBop :: Bop -> SDoc
prettyBop OPPlus = text "+"
prettyBop OPMinus = text "-"
prettyBop OPMultiply = text "*"
prettyBop OPDivide = text "/"
prettyBop OPModulo = text "%"
prettyBop OPPlusAssign = text "+="
prettyBop OPMinusAssign = text "-="
prettyBop OPMultiplyAssign = text "*="
prettyBop OPDivideAssign = text "/="
prettyBop OPModuloAssign = text "%="
prettyBop OPEqual = text "=="
prettyBop OPNotEqual = text "!="
prettyBop OPLogicalAnd = text "&&"
prettyBop OPLogicalOr = text "||"
prettyBop OPLess = text "<"
prettyBop OPLessEqual = text "<="
prettyBop OPGreater = text ">"
prettyBop OPGreaterEqual = text ">="

prettyType :: Type -> SDoc
prettyType TVoid = text "TVoid"
prettyType TInt = text "TInt"
prettyType TFloat = text "TFloat"
prettyType TChar = text "TChar"
prettyType (TIdent s) = text "TIdent" <> parens (doubleQuotes (text s))
prettyType (TPoint t) = text "TPoint" <> parens (prettyType t)
prettyType (TReference t) = text "TReference" <> parens (prettyType t)
prettyType (TFuncPoint returnType paramTypes) =
  text "TFuncPoint"
    <> parens
      ( prettyType returnType
          <> comma
          <+> if null paramTypes
            then text "(TVoid)"
            else parens (vcat (punctuate comma (map prettyType paramTypes)))
      )

prettyExpression :: Expression -> SDoc
prettyExpression (EVar x) = text "EVar" <> parens (doubleQuotes (text x))
prettyExpression (EInt i) = text "EInt" <> parens (int i)
prettyExpression (EChar c) = text "EChar" <+> text [c]
prettyExpression (EString s) = text "EString" <+> text (show s)
prettyExpression (EBinOp bop expressionA expressionB) =
  text "EBinOp"
    <> parens
      ( prettyBop bop
          <> comma
          <+> prettyExpression expressionA
          <> comma
          <+> prettyExpression expressionB
      )
prettyExpression (EUnOp uop expression) =
  text "EUnOp"
    <> parens
      ( prettyUop uop
          <> comma
          <+> prettyExpression expression
      )
prettyExpression (ECall name arguments) =
  text "ECall"
    <> parens
      ( doubleQuotes (text name)
          <> comma
          <+> parens
            (hcat (punctuate (comma <+> text "") (map prettyExpression arguments)))
      )
prettyExpression (ECallExpr calleeExpression arguments) =
  text "ECallExpr"
    <> parens
      ( prettyExpression calleeExpression
          <> comma
          <+> parens
            (hcat (punctuate (comma <+> text "") (map prettyExpression arguments)))
      )
-- EArrayAccess String Expression (Maybe String)
prettyExpression (EArrayAccess arrayExpression indexExpression) =
  text "EArrayAccess"
    <> parens
      ( prettyExpression arrayExpression
          <> comma
          <+> prettyExpression indexExpression
      )
prettyExpression (EReference expression) =
  text "EReference"
    <> parens (prettyExpression expression)
prettyExpression (EDereference expression) =
  text "EDereference"
    <> parens (prettyExpression expression)
prettyExpression (EMemberAccess expression field) =
  text "EMemberAccess"
    <> parens
      ( prettyExpression expression
          <> comma
          <+> doubleQuotes (text field)
      )
prettyExpression (EPointerAccess expression field) =
  text "EPointerAccess"
    <> parens
      ( prettyExpression expression
          <> comma
          <+> doubleQuotes (text field)
      )
prettyExpression (EParen expression) =
  text "EParen" <> parens (prettyExpression expression)
prettyExpression _ = text "<expression> not implemented yet"

prettyStatement :: Statement -> SDoc
prettyStatement (SExpr expression) =
  text "SExpr"
    <> parens
      ( prettyExpression expression
      )
prettyStatement (SVarDecl varType name) =
  text "SVarDecl"
    <> parens
      ( prettyType varType
          <> comma
          <+> doubleQuotes (text name)
      )
prettyStatement (SVarDef varType name expression) =
  text "SVarDef"
    <> parens
      ( prettyType varType
          <> comma
          <+> doubleQuotes (text name)
          <> comma
          <+> prettyExpression expression
      )
prettyStatement (SAssign lhsExpression rhsExpression) =
  text "SAssign"
    <> parens
      ( prettyExpression lhsExpression
          <> comma
          <+> prettyExpression rhsExpression
      )
prettyStatement (SVarAssign name expression) =
  text "SVarAssign"
    <> parens
      ( doubleQuotes (text name)
          <> comma
          <+> prettyExpression expression
      )
prettyStatement (SArrayDecl arrayType name expressions) =
  text "SArrayDecl"
    <> parens
      ( prettyType arrayType
          <> comma
          <+> doubleQuotes (text name)
          <> comma
          <+> parens
            (hcat (punctuate (comma <+> text "") (map prettyExpression expressions)))
      )
prettyStatement (SArrayAssign name index maybeLabel expression) =
  text "SArrayAssign"
    <> parens
      ( doubleQuotes (text name)
          <> comma
          <+> prettyExpression index
          <> comma
          <+> case maybeLabel of
            Nothing -> text ""
            Just label -> doubleQuotes (text label)
          <> comma
          <+> prettyExpression expression
      )
prettyStatement (SScope statements) =
  if null statements
    then text "SScope {}"
    else
      text "SScope"
        <> text "{"
        $$ nest 2 (vcat (punctuate comma (map prettyStatement statements)))
        $$ text "}"
prettyStatement (SIf expression statement maybeStatement) =
  text "SIf"
    <> parens
      ( prettyExpression expression
          <> comma
          <+> prettyStatement statement
          <> comma
          <+> case maybeStatement of
            Nothing -> text ""
            Just elseStatement -> prettyStatement (elseStatement)
      )
prettyStatement (SWhile expression statement) =
  text "SWhile"
    <> parens
      ( prettyExpression expression
          <> comma
          $$ (prettyStatement statement)
      )
prettyStatement (SFor initStatement condExpression updateStatement statement) =
  text "SFor"
    <> parens
      ( prettyStatement initStatement
          <> comma
          <+> prettyExpression condExpression
          <> comma
          <+> prettyStatement updateStatement
          <> comma
          $$ (prettyStatement statement)
      )
prettyStatement (SBreak) =
  text "SBreak"
prettyStatement (SReturn maybeExpression) =
  text "SReturn"
    <> case maybeExpression of
      Nothing -> text ""
      Just expression -> parens (prettyExpression expression)
prettyStatement (SGoto label) =
  text "SGoto"
    <> parens (doubleQuotes (text label))
prettyStatement (SLabel label) =
  text "SLabel"
    <> parens (doubleQuotes (text label))
prettyStatement _ = text "<statement> no implemented yet"

prettyParam :: (Type, String) -> SDoc
prettyParam (paramType, paramName) =
  parens (prettyType paramType <> comma <+> doubleQuotes (text paramName))

prettyGlobal :: Global -> SDoc
prettyGlobal global = case global of
  GFuncDef modifiers returnType name params body ->
    text "GFuncDef"
      <> parens
        ( hcat
            ( punctuate
                (comma <+> text "")
                (map (doubleQuotes . text) modifiers)
            )
            <> comma
            <+> prettyType returnType
            <> comma
            <+> doubleQuotes (text name)
            <> comma
            <+> if null params
              then text "()"
              else parens (vcat (punctuate comma (map prettyParam params)))
        )
      <+> text "{"
      $$ nest 2 (prettyStatement body)
      $$ text "}"

prettyProgram :: Program -> SDoc
prettyProgram (Prog globals) =
  vcat (map prettyGlobal globals)

exampleAST :: Program
exampleAST =
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
                    ( [EInt 4]
                    )
                ),
              SVarDef
                (TPoint (TIdent "channel"))
                "s_out"
                ( ECall
                    "create_buffer_nonblocking"
                    ( [EInt 2]
                    )
                ),
              SVarDef
                (TPoint (TIdent "channel"))
                "s_1"
                ( ECall
                    "create_buffer_nonblocking"
                    ( [EInt 1]
                    )
                ),
              SVarDef
                (TPoint (TIdent "channel"))
                "s_1_delay"
                ( ECall
                    "create_buffer_nonblocking"
                    ( [EInt 1]
                    )
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
                (SReturn (Nothing))
                Nothing,
              SIf
                (EBinOp OPGreater (EVar "i") (EInt 0))
                (SReturn (Just (EInt 1)))
                (Just (SReturn (Just (EInt 0)))),
              SReturn (Just (EInt 0)),
              SReturn (Nothing),
              -- The following are some grammar that can't be achieved by cigrid AST
              -- But now can be used

              -- for (int i=0; i<n; ++i){
              -- printf("%d\n", i);
              -- }
              SFor
                (SVarDef TInt "i" (EInt 0))
                (EBinOp OPLess (EVar "i") (EVar "n"))
                (SExpr (EUnOp OPIncrement (EVar "i")))
                (SScope [SExpr (ECall "printf" [EString "%d\n", EVar "i"])]),
              -- struct fifo & fifo = f;
              SVarDef
                (TReference (TIdent "fifo"))
                "fifo"
                (EVar "f"),
              -- printf("Hi"); // has new way to call
              SExpr (ECallExpr (EVar "printf") [EString "Hi"]),
              -- a->funca()
              SExpr (ECallExpr (EPointerAccess (EVar "a") "funca") []),
              -- a.getB().printB();
              SExpr
                ( ECallExpr
                    ( EMemberAccess
                        (ECallExpr (EMemberAccess (EVar "a") "getB") [])
                        "printB"
                    )
                    []
                ),
              -- p[i]->value;
              SExpr
                ( EPointerAccess
                    (EArrayAccess (EVar "p") (EVar "i"))
                    "value"
                ),
              -- pthread_mutex_lock(&fifo->lock);
              SExpr
                ( ECall
                    "pthread_mutex_lock"
                    [ EReference
                        (EPointerAccess (EVar "fifo") "lock")
                    ]
                ),
              -- int value = *p;
              -- \*p = 10;
              SVarDef
                TInt
                "value"
                (EDereference (EVar "p")),
              SAssign
                (EDereference (EVar "p"))
                (EInt 10),
              -- int input[2];
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
        ( SScope []
        )
    ]

main :: IO ()
main = do
  putStrLn $ showSDocUnsafe $ prettyProgram exampleAST
