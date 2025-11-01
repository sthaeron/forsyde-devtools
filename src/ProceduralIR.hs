module ProceduralIR
  ( Uop (..),
    Bop (..),
    Type (..),
    Expression (..),
    Statement (..),
    Global (..),
    Program (..),
    prettyUop,
    prettyBop,
    prettyType,
    prettyExpression,
    prettyStatement,
    prettyGlobal,
    prettyProgram,
  )
where

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

-- Helper functions for string formatting
parens :: String -> String
parens s = "(" ++ s ++ ")"

brackets :: String -> String
brackets s = "[" ++ s ++ "]"

braces :: String -> String
braces s = "{" ++ s ++ "}"

quotes :: String -> String
quotes s = "\"" ++ s ++ "\""

commaSep :: [String] -> String
commaSep = intercalate ", "

intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x : xs) = x ++ sep ++ intercalate sep xs

indent :: String -> String
indent = unlines . map ("  " ++) . lines

-- Pretty printing functions for ProceduralIR (now returning String)
prettyUop :: Uop -> String
prettyUop OPNegate = "-"
prettyUop OPLogicalNot = "!"
prettyUop OPIncrement = "++"
prettyUop OPDecrement = "--"

prettyBop :: Bop -> String
prettyBop OPPlus = "+"
prettyBop OPMinus = "-"
prettyBop OPMultiply = "*"
prettyBop OPDivide = "/"
prettyBop OPModulo = "%"
prettyBop OPPlusAssign = "+="
prettyBop OPMinusAssign = "-="
prettyBop OPMultiplyAssign = "*="
prettyBop OPDivideAssign = "/="
prettyBop OPModuloAssign = "%="
prettyBop OPEqual = "=="
prettyBop OPNotEqual = "!="
prettyBop OPLogicalAnd = "&&"
prettyBop OPLogicalOr = "||"
prettyBop OPLess = "<"
prettyBop OPLessEqual = "<="
prettyBop OPGreater = ">"
prettyBop OPGreaterEqual = ">="

prettyType :: Type -> String
prettyType TVoid = "TVoid"
prettyType TInt = "TInt"
prettyType TFloat = "TFloat"
prettyType TChar = "TChar"
prettyType (TIdent s) = "TIdent" ++ parens (quotes s)
prettyType (TPoint t) = "TPoint" ++ parens (prettyType t)
prettyType (TReference t) = "TReference" ++ parens (prettyType t)
prettyType (TFuncPoint returnType paramTypes) =
  "TFuncPoint"
    ++ parens
      ( prettyType returnType
          ++ ", "
          ++ if null paramTypes
            then "(TVoid)"
            else parens (commaSep (map prettyType paramTypes))
      )

prettyExpression :: Expression -> String
prettyExpression (EVar x) = "EVar" ++ parens (quotes x)
prettyExpression (EInt i) = "EInt" ++ parens (show i)
prettyExpression (EChar c) = "EChar " ++ show c
prettyExpression (EString s) = "EString " ++ show s
prettyExpression (EBinOp bop expressionA expressionB) =
  "EBinOp"
    ++ parens
      ( prettyBop bop
          ++ ", "
          ++ prettyExpression expressionA
          ++ ", "
          ++ prettyExpression expressionB
      )
prettyExpression (EUnOp uop expression) =
  "EUnOp"
    ++ parens
      ( prettyUop uop
          ++ ", "
          ++ prettyExpression expression
      )
prettyExpression (ECall name arguments) =
  "ECall"
    ++ parens
      ( quotes name
          ++ ", "
          ++ parens (commaSep (map prettyExpression arguments))
      )
prettyExpression (ECallExpr calleeExpression arguments) =
  "ECallExpr"
    ++ parens
      ( prettyExpression calleeExpression
          ++ ", "
          ++ parens (commaSep (map prettyExpression arguments))
      )
prettyExpression (EArrayAccess arrayExpression indexExpression) =
  "EArrayAccess"
    ++ parens
      ( prettyExpression arrayExpression
          ++ ", "
          ++ prettyExpression indexExpression
      )
prettyExpression (EReference expression) =
  "EReference" ++ parens (prettyExpression expression)
prettyExpression (EDereference expression) =
  "EDereference" ++ parens (prettyExpression expression)
prettyExpression (EMemberAccess expression field) =
  "EMemberAccess"
    ++ parens
      ( prettyExpression expression
          ++ ", "
          ++ quotes field
      )
prettyExpression (EPointerAccess expression field) =
  "EPointerAccess"
    ++ parens
      ( prettyExpression expression
          ++ ", "
          ++ quotes field
      )
prettyExpression (EParen expression) =
  "EParen" ++ parens (prettyExpression expression)
prettyExpression _ = "<expression> not implemented yet"

prettyStatement :: Statement -> String
prettyStatement (SExpr expression) =
  "SExpr" ++ parens (prettyExpression expression)
prettyStatement (SVarDecl varType name) =
  "SVarDecl"
    ++ parens
      ( prettyType varType
          ++ ", "
          ++ quotes name
      )
prettyStatement (SVarDef varType name expression) =
  "SVarDef"
    ++ parens
      ( prettyType varType
          ++ ", "
          ++ quotes name
          ++ ", "
          ++ prettyExpression expression
      )
prettyStatement (SAssign lhsExpression rhsExpression) =
  "SAssign"
    ++ parens
      ( prettyExpression lhsExpression
          ++ ", "
          ++ prettyExpression rhsExpression
      )
prettyStatement (SVarAssign name expression) =
  "SVarAssign"
    ++ parens
      ( quotes name
          ++ ", "
          ++ prettyExpression expression
      )
prettyStatement (SArrayDecl arrayType name expressions) =
  "SArrayDecl"
    ++ parens
      ( prettyType arrayType
          ++ ", "
          ++ quotes name
          ++ ", "
          ++ parens (commaSep (map prettyExpression expressions))
      )
prettyStatement (SArrayAssign name index maybeLabel expression) =
  "SArrayAssign"
    ++ parens
      ( quotes name
          ++ ", "
          ++ prettyExpression index
          ++ ", "
          ++ case maybeLabel of
            Nothing -> ""
            Just label -> quotes label
          ++ ", "
          ++ prettyExpression expression
      )
prettyStatement (SScope statements) =
  if null statements
    then "SScope {}"
    else
      "SScope {"
        ++ "\n"
        ++ indent (intercalate ",\n" (map prettyStatement statements))
        ++ "\n}"
prettyStatement (SIf expression statement maybeStatement) =
  "SIf"
    ++ parens
      ( prettyExpression expression
          ++ ", "
          ++ prettyStatement statement
          ++ ", "
          ++ case maybeStatement of
            Nothing -> ""
            Just elseStatement -> prettyStatement elseStatement
      )
prettyStatement (SWhile expression statement) =
  "SWhile"
    ++ parens
      ( prettyExpression expression
          ++ ", "
          ++ prettyStatement statement
      )
prettyStatement (SFor initStatement condExpression updateStatement statement) =
  "SFor"
    ++ parens
      ( prettyStatement initStatement
          ++ ", "
          ++ prettyExpression condExpression
          ++ ", "
          ++ prettyStatement updateStatement
          ++ ", "
          ++ prettyStatement statement
      )
prettyStatement SBreak = "SBreak"
prettyStatement (SReturn maybeExpression) =
  "SReturn"
    ++ case maybeExpression of
      Nothing -> ""
      Just expression -> parens (prettyExpression expression)
prettyStatement (SGoto label) = "SGoto" ++ parens (quotes label)
prettyStatement (SLabel label) = "SLabel" ++ parens (quotes label)
prettyStatement _ = "<statement> not implemented yet"

prettyParam :: (Type, String) -> String
prettyParam (paramType, paramName) =
  parens (prettyType paramType ++ ", " ++ quotes paramName)

prettyGlobal :: Global -> String
prettyGlobal (GFuncDef modifiers returnType name params body) =
  "GFuncDef"
    ++ parens
      ( intercalate ", " (map quotes modifiers)
          ++ ", "
          ++ prettyType returnType
          ++ ", "
          ++ quotes name
          ++ ", "
          ++ if null params
            then "()"
            else parens (commaSep (map prettyParam params))
      )
    ++ " {\n"
    ++ indent (prettyStatement body)
    ++ "\n}"

prettyProgram :: Program -> String
prettyProgram (Prog globals) = intercalate "\n\n" (map prettyGlobal globals)
