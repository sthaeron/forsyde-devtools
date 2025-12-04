module ProceduralIR
  ( UnaryOperator (..),
    BinaryOperator (..),
    Type (..),
    Expression (..),
    Statement (..),
    Global (..),
    Program (..),
    StorageClass (..),
    TypeQualifier (..),
    prettyStorageClass,
    prettyTypeQualifier,
    prettyBinaryOperator,
    prettyUnaryOperator,
    prettyType,
    prettyExpression,
    prettyStatement,
    prettyGlobal,
    prettyProgram,
  )
where

import Text.Printf (printf)

data StorageClass
  = Auto
  | Register
  | Static
  | Extern
  | TypeDefinition

data TypeQualifier
  = Const
  | Restrict
  | Volatile

data UnaryOperator
  = Negate -- -rhs
  | LogicalNot -- !rhs
  | Increment -- ++rhs
  | Decrement -- --rhs
  deriving (Show)

data BinaryOperator
  = Plus -- lhs + rhs
  | Minus -- lhs - rhs
  | Multiply -- lhs * rhs
  | Divide -- lhs / rhs
  | Modulo -- lhs % rhs
  | PlusAssign -- lhs += rhs
  | MinusAssign -- lhs -= rhs
  | MultiplyAssign -- lhs *= rhs
  | DivideAssign -- lhs /= rhs
  | ModuloAssign -- lhs %= rhs
  | Equal -- lhs == rhs
  | NotEqual -- lhs != rhs
  | LogicalAnd -- lhs && rhs
  | LogicalOr -- lhs || rhs
  | Less -- lhs < rhs
  | LessEqual -- lhs <= rhs
  | Greater -- lhs > rhs
  | GreaterEqual -- lhs >= rhs
  deriving (Show)

-- Types
data Type
  = TVoid
  | TInt
  | TFloat
  | TChar
  | TIdent String
  | TPointer Type -- int *pointer
  | TReference Type -- int &pointer
  | TFunctionPointer Type [Type] -- int (*pointer)(int, int)
  | TQualifiedType [TypeQualifier] Type

-- Expressions
data Expression
  = EVar String
  | EInt Int
  | EChar Char
  | EString String
  | EBinOp BinaryOperator Expression Expression
  | EUnOp UnaryOperator Expression
  | ECall String [Expression] -- string(), simple function call
  | ECallExpr Expression [Expression] -- abc.foo(), enable more complex function call
  | EArrayAccess Expression Expression -- expr[expr], modified from cigrid for better expressiveness
  | EReference Expression -- &expr
  | EDereference Expression -- (*expr)
  | EMemberAccess Expression String -- expr.string
  | EPointerAccess Expression String -- expr->string
  | EParen Expression -- (expr)
  deriving (Show)

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
  = GFuncDeclare (Maybe StorageClass) Type String [(Type, String)]
  | GFuncDef (Maybe StorageClass) Type String [(Type, String)] Statement
  | GVarDeclare Type String
  | GVarDef Type String Expression
  | GStruct String [(Type, String)]

data Program = Prog [Global]

-- Helper functions for string formatting
parens :: String -> String
parens s = "(" ++ s ++ ")"

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
prettyUnaryOperator :: UnaryOperator -> String
prettyUnaryOperator Negate = "-"
prettyUnaryOperator LogicalNot = "!"
prettyUnaryOperator Increment = "++"
prettyUnaryOperator Decrement = "--"

prettyBinaryOperator :: BinaryOperator -> String
prettyBinaryOperator Plus = "+"
prettyBinaryOperator Minus = "-"
prettyBinaryOperator Multiply = "*"
prettyBinaryOperator Divide = "/"
prettyBinaryOperator Modulo = "%"
prettyBinaryOperator PlusAssign = "+="
prettyBinaryOperator MinusAssign = "-="
prettyBinaryOperator MultiplyAssign = "*="
prettyBinaryOperator DivideAssign = "/="
prettyBinaryOperator ModuloAssign = "%="
prettyBinaryOperator Equal = "=="
prettyBinaryOperator NotEqual = "!="
prettyBinaryOperator LogicalAnd = "&&"
prettyBinaryOperator LogicalOr = "||"
prettyBinaryOperator Less = "<"
prettyBinaryOperator LessEqual = "<="
prettyBinaryOperator Greater = ">"
prettyBinaryOperator GreaterEqual = ">="

prettyType :: Type -> String
prettyType currentType = case currentType of
  TVoid -> "TVoid"
  TInt -> "TInt"
  TFloat -> "TFloat"
  TChar -> "TChar"
  TIdent tyId -> printf "TIdent(%s)" (quotes tyId)
  TPointer ty -> printf "TPointer(%s)" (prettyType ty)
  TReference ty -> printf "TReference(%s)" (prettyType ty)
  TFunctionPointer ty typeParameters -> printf "TFunctionPointer(%s, {%s})" (prettyType ty) (intercalate ", " (map prettyType typeParameters))
  TQualifiedType qualifers ty -> printf "%s %s" (intercalate " " (map (prettyTypeQualifier) qualifers)) (prettyType ty)

prettyExpression :: Expression -> String
prettyExpression (EVar x) = "EVar" ++ parens (quotes x)
prettyExpression (EInt i) = "EInt" ++ parens (show i)
prettyExpression (EChar c) = "EChar" ++ parens (show c)
prettyExpression (EString s) = "EString" ++ parens (show s)
prettyExpression (EBinOp bop expressionA expressionB) =
  "EBinOp"
    ++ parens
      ( prettyBinaryOperator bop
          ++ ", "
          ++ prettyExpression expressionA
          ++ ", "
          ++ prettyExpression expressionB
      )
prettyExpression (EUnOp uop expression) =
  "EUnOp"
    ++ parens
      ( prettyUnaryOperator uop
          ++ ", "
          ++ prettyExpression expression
      )
prettyExpression (ECall name arguments) =
  "ECall"
    ++ parens
      ( quotes name
          ++ ", "
          ++ braces (commaSep (map prettyExpression arguments))
      )
prettyExpression (ECallExpr calleeExpression arguments) =
  "ECallExpr"
    ++ parens
      ( prettyExpression calleeExpression
          ++ ", "
          ++ braces (commaSep (map prettyExpression arguments))
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
          ++ braces (commaSep (map prettyExpression expressions))
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
    then "SScope({})"
    else
      "SScope({"
        ++ "\n"
        ++ indent (intercalate ",\n" (map prettyStatement statements))
        ++ "})"
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

prettyParam :: (Type, String) -> String
prettyParam (paramType, paramName) =
  parens (prettyType paramType ++ ", " ++ quotes paramName)

prettyTypeQualifier :: TypeQualifier -> String
prettyTypeQualifier qualifier = case qualifier of
  Const -> "const"
  Restrict -> "restrict"
  Volatile -> "volatile"

prettyStorageClass :: StorageClass -> String
prettyStorageClass storageClass = case storageClass of
  Auto -> "auto"
  Register -> "register"
  Static -> "static"
  Extern -> "extern"
  TypeDefinition -> "typedef"

prettyGlobal :: Global -> String
prettyGlobal global = case global of
  GFuncDeclare (Just storageClass) returnType fundId parameters ->
    printf
      "GFuncDeclare(%s, %s, %s, {%s})"
      (prettyStorageClass storageClass)
      (prettyType returnType)
      (quotes fundId)
      (intercalate ", " (map prettyParam parameters))
  GFuncDeclare Nothing returnType funcId parameters ->
    printf
      "GFuncDeclare(%s, %s, {%s})"
      (prettyType returnType)
      (quotes funcId)
      (intercalate ", " (map prettyParam parameters))
  GFuncDef (Just storageClass) returnType funcId parameters body ->
    printf
      "GFuncDef(%s, %s, %s, {%s}, %s)"
      (prettyStorageClass storageClass)
      (prettyType returnType)
      (quotes funcId)
      (intercalate ", " (map prettyParam parameters))
      (prettyStatement body)
  GFuncDef Nothing returnType funcId parameters body ->
    printf
      "GFuncDef(%s, %s, {%s}, %s)"
      (prettyType returnType)
      (quotes funcId)
      (intercalate ", " (map prettyParam parameters))
      (prettyStatement body)
  GVarDeclare varType varId ->
    printf
      "GVarDeclare(%s, %s)"
      (prettyType varType)
      (quotes varId)
  GVarDef varType varId expression ->
    printf
      "GVarDef(%s, %s, %s)"
      (prettyType varType)
      (quotes varId)
      (prettyExpression expression)
  GStruct structId fields ->
    printf
      "GStruct(%s {\n%s})"
      (quotes structId)
      (intercalate ",\n" (map prettyParam fields))

prettyProgram :: Program -> String
prettyProgram (Prog globals) = intercalate "\n\n" (map prettyGlobal globals) <> "\n"
