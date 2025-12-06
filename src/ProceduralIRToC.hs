module ProceduralIRToC where

import ArgumentsMain (InputType (Predefined, StdIn), Target (PC, PICO2))
import Data.List (intercalate)
import ProceduralIR
import Text.Printf (printf)

indent :: String -> String
indent = unlines . map ("    " ++) . lines

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
translateType currentType = case currentType of
  TVoid -> "void"
  TInt -> "int"
  TFloat -> "float"
  TChar -> "char"
  TIdent s -> s
  TPointer t -> translateType t ++ " *"
  TReference t -> translateType t ++ "&"
  TQualifiedType qualifers ty -> (intercalate " " (map prettyTypeQualifier qualifers)) ++ (translateType ty)
  TFunctionPointer returnType paramTypes ->
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
translateExpression (EUnOp unop@Increment expr) =
  translateExpression expr ++ translateUnaryOperator unop
translateExpression (EUnOp unop@Decrement expr) =
  translateExpression expr ++ translateUnaryOperator unop
translateExpression (EUnOp unop expr) =
  translateUnaryOperator unop ++ translateExpression expr
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
translateStatement (SVarDecl varType@(TPointer _pointerType) name) =
  translateType varType ++ name ++ ";"
translateStatement (SVarDecl varType name) =
  translateType varType ++ " " ++ name ++ ";"
translateStatement (SVarDef varType@(TPointer _pointerType) name expression) =
  translateType varType ++ name ++ " = " ++ translateExpression expression ++ ";"
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
  "while (" ++ translateExpression expression ++ ") " ++ translateStatement statement
translateStatement (SFor initStmt condExpr updateStmt bodyStmt) =
  "for ("
    ++ stripSemicolon (translateStatement initStmt)
    ++ "; "
    ++ translateExpression condExpr
    ++ "; "
    ++ stripSemicolon (translateStatement updateStmt)
    ++ ") "
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
translateParam (paramType@(TPointer _pointerType), paramName) =
  translateType paramType ++ paramName
translateParam (paramType, paramName) =
  translateType paramType ++ " " ++ paramName

translateGlobal :: Global -> String
translateGlobal global = case global of
  GFuncDeclare (Just storageClass) returnType funcId parameters ->
    printf
      "%s %s %s(%s);"
      (prettyStorageClass storageClass)
      (translateType returnType)
      (funcId)
      (intercalate ", " (map translateParam parameters))
  GFuncDeclare Nothing returnType funcId parameters ->
    printf
      "%s %s(%s);"
      (translateType returnType)
      (funcId)
      (intercalate ", " (map translateParam parameters))
  GFuncDef (Just storageClass) returnType funcId parameters body ->
    printf
      "%s %s %s(%s) %s"
      (prettyStorageClass storageClass)
      (translateType returnType)
      (funcId)
      (intercalate ", " (map translateParam parameters))
      (translateStatement body)
  GFuncDef Nothing returnType funcId parameters body ->
    printf
      "%s %s(%s) %s"
      (translateType returnType)
      (funcId)
      (intercalate ", " (map translateParam parameters))
      (translateStatement body)
  GVarDeclare varType varId ->
    printf
      "%s %s;"
      (translateType varType)
      (varId)
  GVarDef varType varId expression ->
    printf
      "%s %s = %s;"
      (translateType varType)
      (varId)
      (translateExpression expression)
  GStruct structId fields ->
    printf
      "%s {\n%s};"
      (structId)
      (intercalate ",\n" (map translateParam fields))

translateProgram :: Program -> Target -> InputType -> Bool -> String
translateProgram (Prog globals) target io includes =
  let targetText = case target of
        PC -> "#define PLATFORM PC\n"
        PICO2 -> "#define PLATFORM PICO2\n"
      includeInput = case io of
        StdIn -> ""
        Predefined -> "#include \"input.h\"\n"
   in let outputProgram =
            if includes
              then
                targetText
                  ++ "typedef int token;\n#include \"include/common.h\"\n#include <stdio.h>\n"
                  ++ includeInput
                  ++ "\n"
                  ++ intercalate "\n\n" (map translateGlobal globals)
              else
                intercalate "\n\n" (map translateGlobal globals)
       in outputProgram ++ "\n"
