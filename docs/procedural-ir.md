# Procedural IR Documentation


## Overview
Procedural IR is a set of data structures representing C code abstract syntax, used for translating ForSyDe IR and C code generation. It is adapted from the Cigrid Abstract Syntax Tree, with modification and simplification to improve expressiveness and readability.

## Design principles
The original Cigrid AST was designed to represent C programs with a focus on precise, unambiguous translation into low-level or assembly-like representations. In contrast, Procedural IR focuses on reconstructing the syntatic structure of C code in a structured form suitable for C code generation. To achieve this goal, several modification and simplification are introduced from the original Cigrid AST to improve expressiveness. While a pure token stream is also enough for generating C code, an AST structure has better readability and extendability for complex code structures.

It is important to note that this IR contains some syntax that are redundant from a formal C syntax perspective. For example, the distinction between `ECall` (from Cigrid) and `ECallExpr` (added), or the various assignment forms like `SVarAssign` `SArrayAssign`(from Cigrid), and `SAssign`(added), represent semantically equivalent concepts in C syntax. This AST is not suitable for a conventional C compiler because these redundancies would create serious ambiguity issues. However, since this representation serves as a tool for C code generation in a simpler manner, such duplications do not pose problems and instead provide flexibility in code construction.

Additionally, the procedural IR serves as an intermediate representation when converting from ForSyDeIR, which primarily captures ForSyDe model structure and the functions executed by actors. Consequently:

- C constructs such as `enum`s, `#include` directives, and `typedef`s are treated as macros for now and taken care of during the C code generation phase.

- These language elements are intentionally excluded from the current IR stage to maintain focus on the core model semantics.


## Key Components

### 1. StorageClass and TypeQualifier
```Haskell
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
```

These are the C keywords of storage class and type qualifiers.

### 2. Unary and Binary Operators
``` Haskell
data UnaryOperator
  = Negate -- -rhs
  | LogicalNot -- !rhs
  | Increment -- ++rhs
  | Decrement -- --rhs

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
```

Procedural IR does not contain information about the associativity and precedence of operators. Specifically, unary operators are currently all treated the same way and placed at the left of the expression. This is currently fine since `++i` works in loop, but use with care if using this grammar in array access. 


### 3. Types (`Type`)
```Haskell
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
```

C types supported by Procedural IR include:

* Primitive types (`int`, `char`,  `float`, `void`)
* Struct types (`struct Type`)
* Pointers and references (`Type*`, `Type&`)
* Function pointers (`ReturnType (*func)(ParamTypes)`)


### 4. Expressions (`Expression`)

```Haskell
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
```

Expressions can be considered as values, including literals, variables and its references and pointers, arithmetic, and function calls. 


### 5. Statements (`Statement`)
```Haskell
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
```
Statements are the main building blocks of procedural program with assignments, control flow.


### 6. Globals
```Haskell
-- Globals
data Global
  = GFuncDeclare (Maybe StorageClass) Type String [(Type, String)]
  | GFuncDef (Maybe StorageClass) Type String [(Type, String)] Statement
  | GVarDeclare Type String
  | GVarDef Type String Expression
  | GStruct String [(Type, String)]
```

### 7. Programs (`Program`)

```Haskell
data Program = Prog [Global]
```

A complete Procedural IR program is a list of Global (functions).

## Limitations and future work

### Unsupported C Language features

**1. Arrays and Parameters**
- If a function takes an array as parameter, it can only be represented by `int *x`, not `int x[]`, since currently function parameters are only represented as `Type Name`.
- Due to the reason above, multi-dimensional arrays as parameters (e.g., `int x[][4]`) are not handled. Only dynamically allocated arrays could be used and pass them as `int**`. If this is a necessity, it can be updated in future.

**2. Ternary operators**  
The following code from `SDF_example_001.c` is not supported:
```
count_a = fifo->head + count <= fifo->size ? count : fifo->size - fifo->head;
```


## Note: things need to take care of in C codegen

- Printing function pointer parameters may introduce ambiguity and differs from printing other types. It is not simply a matter of printing the type followed by the variable name, as the variable name is embedded within the type syntax. For example:
    - `void *f(token*, token*)` represents a function returning a pointer.
    - `void (*f)(token*, token*)` represents a function pointer variable.
    
    Since our mainly usage of function pointer as function parameters and it is rare to use it in other places, this can also be addressed in later stage by defining an explicit template of printing parameters with type `TFunctionPointer`.


