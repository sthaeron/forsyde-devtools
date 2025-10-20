module ProceduralIR where

import GHC.Utils.Outputable

data Uop

data Bop

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

prettyType :: Type -> SDoc
prettyType TVoid = text "TVoid"
prettyType TInt = text "TInt"
prettyType TChar = text "TChar"
prettyType (TIdent s) = text "TIdent" <+> parens (doubleQuotes (text s))
prettyType (TPoint t) = text "TPoint" <+> parens (prettyType t)

prettyExpression :: Expression -> SDoc
prettyExpression (EVar x) = text "EVar" <+> doubleQuotes (text x)
prettyExpression (EInt i) = text "EInt" <+> int i
prettyExpression (EChar c) = text "EChar" <+> text [c]
prettyExpression (EString s) = text "EString" <+> text s
prettyExpression (ECall name expressions) =
  text "ECall"
    <+> parens
      ( doubleQuotes (text name)
          <+> comma
          <+> parens
            (hcat (punctuate (comma <+> text "") (map prettyExpression expressions)))
      )
prettyExpression _ = text "<expression> not implemented yet"

prettyStatement :: Statement -> SDoc
prettyStatement (SExpr expression) =
  text "SExpr"
    <+> parens
      ( prettyExpression expression
      )
prettyStatement (SVarDecl varType name) =
  text "SVarDecl"
    <+> parens
      ( prettyType varType
          <+> comma
          <+> doubleQuotes (text name)
      )
prettyStatement (SVarDef varType name expression) =
  text "SVarDef"
    <+> parens
      ( prettyType varType
          <+> comma
          <+> doubleQuotes (text name)
          <+> comma
          <+> prettyExpression expression
      )
prettyStatement (SScope statements) =
  if null statements
    then text "SScope {}"
    else
      text "SScope"
        <+> text "{"
        $$ nest 2 (vcat (punctuate comma (map prettyStatement statements)))
        $$ text "}"
prettyStatement (SWhile expression statement) =
  text "SWhile"
    <+> parens
      ( prettyExpression expression
          <+> comma
          $$ (prettyStatement statement)
      )
prettyStatement _ = text "<statement> no implemented yet"

prettyParam :: (Type, String) -> SDoc
prettyParam (paramType, paramName) =
  parens (prettyType paramType <+> comma <+> doubleQuotes (text paramName))

prettyGlobal :: Global -> SDoc
prettyGlobal global = case global of
  GFuncDef returnType name params body ->
    text "GFuncDef"
      <+> parens
        ( prettyType returnType
            <+> comma
            <+> doubleQuotes (text name)
            <+> comma
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
                )
            ]
        )
    ]

main :: IO ()
main = do
  putStrLn $ showSDocUnsafe $ prettyProgram exampleAST
