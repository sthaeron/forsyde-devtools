module ProceduralIR where

import GHC.Utils.Outputable
import Prelude hiding ((<>))

data Uop
  = OPNegate
  | OPLogicalNot
  | OPIncrement
  | OPDecrement

data Bop
  = OPPlus
  | OPMinus
  | OPMultiply
  | OPDivide
  | OPEqual
  | OPNotEqual
  | OPLogicalAnd
  | OPLogicalOr
  | OPLess
  | OPLessEqual
  | OPGreater
  | OPGreaterEqual

-- T ::= TVoid | TInt | TChar | TIdent(r) | TPoint(T)
data Type
  = TVoid
  | TInt
  | TChar
  | TIdent String
  | TPoint Type

-- Expressions
-- e ::= EVar(r) | EInt(i) | EChar(c) | EString(r)
--     | EBinOp(bop, e, e) | EUnOp(uop, e) | ECall(r, e)
--     | ENew(T, e) | EArrayAccess(r, e, rˆ)
data Expression
  = EVar String
  | EInt Int
  | EChar Char
  | EString String
  | EBinOp Bop Expression Expression
  | EUnOp Uop Expression
  | ECall String [Expression]
  | ENew Type Expression
  | EArrayAccess String Expression (Maybe String)

-- Statements
-- s ::= SExpr(e) | SVarDef(T, r, e) | SVarAssign(r, e)
--     | SArrayAssign(r, e, r, eˆ) | SScope(s) | SIf(e, s, sˆ)
--     | SWhile(e, s) | SBreak | SReturn(ˆe) | SDelete(r)
data Statement
  = SExpr Expression
  | SVarDecl Type String -- Added for: Token input; int i;
  | SVarDef Type String Expression
  | SVarAssign String Expression
  | SArrayAssign String Expression (Maybe String) Expression
  | SScope [Statement]
  | SIf Expression Statement (Maybe Statement)
  | SWhile Expression Statement
  | SFor Statement Expression Statement Statement
  | SBreak
  | SReturn (Maybe Expression)
  | SDelete String

-- Globals
-- g ::= GFuncDef(T, r, (T, r), s) | GFuncDecl(T, r, (T, r))
--     | GVarDef(T, r, e) | GVarDecl(T, r) | GStruct(r, (T, r))
data Global
  = GFuncDef Type String [(Type, String)] Statement

-- The following might not be needed
-- \| GFuncDecl Type String [(Type, String)]
-- \| GVarDef Type String Expression
-- \| GVarDecl Type String
-- \| GStruct String [(Type, String)]

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
prettyType TChar = text "TChar"
prettyType (TIdent s) = text "TIdent" <> parens (doubleQuotes (text s))
prettyType (TPoint t) = text "TPoint" <> parens (prettyType t)

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
prettyExpression (ECall name expressions) =
  text "ECall"
    <> parens
      ( doubleQuotes (text name)
          <> comma
          <+> parens
            (hcat (punctuate (comma <+> text "") (map prettyExpression expressions)))
      )
prettyExpression (ENew varType expression) =
  text "ENew"
    <> parens
      ( prettyType varType
          <> comma
          <+> prettyExpression expression
      )
-- EArrayAccess String Expression (Maybe String)
prettyExpression (EArrayAccess name expression maybeLabel) =
  text "EArrayAccess"
    <> parens
      ( doubleQuotes (text name)
          <> comma
          <+> prettyExpression expression
          <> comma
          <+> case maybeLabel of
            Nothing -> text ""
            Just label -> doubleQuotes (text label)
      )
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
prettyStatement (SVarAssign name expression) =
  text "SVarAssign"
    <> parens
      ( doubleQuotes (text name)
          <> comma
          <+> prettyExpression expression
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
prettyStatement (SDelete name) =
  text "SDelete"
    <> parens (doubleQuotes (text name))
prettyStatement _ = text "<statement> no implemented yet"

prettyParam :: (Type, String) -> SDoc
prettyParam (paramType, paramName) =
  parens (prettyType paramType <> comma <+> doubleQuotes (text paramName))

prettyGlobal :: Global -> SDoc
prettyGlobal global = case global of
  GFuncDef returnType name params body ->
    text "GFuncDef"
      <> parens
        ( prettyType returnType
            <> comma
            <+> doubleQuotes (text name)
            <> comma
            <+> parens (hcat (punctuate comma (map prettyParam params)))
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
              SFor
                (SVarDef TInt "i" (EInt 0))
                (EBinOp OPLess (EVar "i") (EVar "n"))
                (SExpr (EUnOp OPIncrement (EVar "i")))
                (SScope [SExpr (ECall "printf" [EString "%d\n", EVar "i"])]),
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
              SReturn (Nothing)
            ]
        )
    ]

main :: IO ()
main = do
  putStrLn $ showSDocUnsafe $ prettyProgram exampleAST
