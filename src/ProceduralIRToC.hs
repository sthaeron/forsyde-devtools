module ProceduralIRToC where

import Data.List (intercalate)
import ProceduralIR
import System.IO
import System.Process (readProcess)
import Text.Printf (printf)

indent :: String -> String
indent = unlines . map ("  " ++) . lines

stripSemicolon :: String -> String
stripSemicolon str =
  if not (null str) && last str == ';'
    then init str
    else str

translateUnaryOperator :: UnaryOperator -> String
translateUnaryOperator Negate = "-"
translateUnaryOperator LogicalNot = "!"
translateUnaryOperator Increment = "++"
translateUnaryOperator Decrement = "--"

translateBinaryOperator :: BinaryOperator -> String
translateBinaryOperator Plus = "+"
translateBinaryOperator Minus = "-"
translateBinaryOperator Multiply = "*"
translateBinaryOperator Divide = "/"
translateBinaryOperator Modulo = "%"
translateBinaryOperator PlusAssign = "+="
translateBinaryOperator MinusAssign = "-="
translateBinaryOperator MultiplyAssign = "*="
translateBinaryOperator DivideAssign = "/="
translateBinaryOperator ModuloAssign = "%="
translateBinaryOperator Equal = "=="
translateBinaryOperator NotEqual = "!="
translateBinaryOperator LogicalAnd = "&&"
translateBinaryOperator LogicalOr = "||"
translateBinaryOperator Less = "<"
translateBinaryOperator LessEqual = "<="
translateBinaryOperator Greater = ">"
translateBinaryOperator GreaterEqual = ">="

translateType :: Type -> String
translateType TVoid = "void"
translateType TInt = "int"
translateType TFloat = "float"
translateType TChar = "char"
translateType (TIdent s) = s
translateType (TPoint t) = translateType t ++ "*"
translateType (TReference t) = translateType t ++ "&"
translateType (TFuncPoint returnType paramTypes) =
  translateType returnType
    ++ "(*)("
    ++ ( if null paramTypes
           then "void"
           else intercalate ", " (map translateType paramTypes)
       )
    ++ ")"

translateExpression :: Expression -> String
translateExpression (EVar x) = x
translateExpression (EInt i) = show i
translateExpression (EChar c) = show c
translateExpression (EString s) = show s
translateExpression (EBinOp bop exprA exprB) =
  translateExpression exprA ++ " " ++ translateBinaryOperator bop ++ " " ++ translateExpression exprB
translateExpression (EUnOp uop expr) =
  translateUnaryOperator uop ++ translateExpression expr
translateExpression (ECall name arguments) =
  name ++ "(" ++ intercalate ", " (map translateExpression arguments) ++ ")"
translateExpression (ECallExpr calleeExpr arguments) =
  translateExpression calleeExpr ++ "(" ++ intercalate ", " (map translateExpression arguments) ++ ")"
translateExpression (EArrayAccess arrayExpr indexExpr) =
  translateExpression arrayExpr ++ "[" ++ translateExpression indexExpr ++ "]"
translateExpression (EReference expr) =
  "&" ++ translateExpression expr
translateExpression (EDereference expr) =
  "*" ++ translateExpression expr
translateExpression (EMemberAccess expr field) =
  translateExpression expr ++ "." ++ field
translateExpression (EPointerAccess expr field) =
  translateExpression expr ++ "->" ++ field
translateExpression (EParen expr) =
  "(" ++ translateExpression expr ++ ")"

translateStatement :: Statement -> String
translateStatement (SExpr expression) =
  translateExpression expression ++ ";"
translateStatement (SVarDecl varType name) =
  translateType varType ++ " " ++ name ++ ";"
translateStatement (SVarDef varType name expression) =
  translateType varType ++ " " ++ name ++ " = " ++ translateExpression expression ++ ";"
translateStatement (SAssign lhsExpression rhsExpression) =
  translateExpression lhsExpression ++ " = " ++ translateExpression rhsExpression ++ ";"
translateStatement (SVarAssign name expression) =
  name ++ " = " ++ translateExpression expression ++ ";"
translateStatement (SArrayDecl arrayType name expressions) =
  translateType arrayType
    ++ " "
    ++ name
    ++ concatMap (\expr -> "[" ++ translateExpression expr ++ "]") expressions
    ++ ";"
translateStatement (SArrayAssign name index maybeLabel expression) =
  let theLabel = case maybeLabel of
        Nothing -> ""
        Just label -> "." ++ label
   in name ++ "[" ++ translateExpression index ++ "]" ++ theLabel ++ " = " ++ translateExpression expression ++ ";"
translateStatement (SScope statements) =
  "{\n" ++ indent (unlines (map translateStatement statements)) ++ "}"
translateStatement (SIf expression thenStmt maybeElseStmt) =
  let thenPart =
        "if ("
          ++ translateExpression expression
          ++ ") {\n"
          ++ indent (translateStatement thenStmt)
          ++ "}"
   in case maybeElseStmt of
        Nothing -> thenPart
        Just elseStmt ->
          thenPart
            ++ " else {\n"
            ++ indent (translateStatement elseStmt)
            ++ "}"
translateStatement (SWhile expression statement) =
  "while (" ++ translateExpression expression ++ ")\n" ++ translateStatement statement
translateStatement (SFor initStmt condExpr updateStmt bodyStmt) =
  "for ("
    ++ stripSemicolon (translateStatement initStmt)
    ++ "; "
    ++ translateExpression condExpr
    ++ "; "
    ++ stripSemicolon (translateStatement updateStmt)
    ++ ")\n"
    ++ translateStatement bodyStmt
translateStatement SBreak =
  "break;"
translateStatement (SReturn maybeExpression) =
  case maybeExpression of
    Nothing -> "return;"
    Just expr -> "return " ++ translateExpression expr ++ ";"
translateStatement (SGoto label) =
  "goto " ++ label ++ ";"
translateStatement (SLabel label) =
  label ++ ":"

translateParam :: (Type, String) -> String
translateParam (paramType, paramName) =
  translateType paramType ++ " " ++ paramName

translateGlobal :: Global -> String
translateGlobal (GFuncDef (Just storageClass) returnType name params body) =
  (prettyStorageClass storageClass)
    ++ translateType returnType
    ++ " "
    ++ name
    ++ "("
    ++ intercalate ", " (map translateParam params)
    ++ ")"
    ++ translateStatement body
translateGlobal (GFuncDef Nothing returnType name params body) =
  translateType returnType
    ++ " "
    ++ name
    ++ "("
    ++ intercalate ", " (map translateParam params)
    ++ ")"
    ++ translateStatement body

translateProgram :: Program -> String
translateProgram (Prog globals) =
  intercalate "\n" (map translateGlobal globals)

formatWithClang :: String -> IO String
formatWithClang code =
  readProcess "clang-format" ["--style=LLVM"] code
