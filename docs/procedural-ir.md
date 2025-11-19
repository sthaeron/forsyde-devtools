# Procedural IR Documentation


## Overview
Procedural IR is a set of data structures representing C code abstract syntax, used for translating ForSyDe IR and C code generation. It is adapted from the Cigrid Abstract Syntax Tree, with modification and simplification to improve expressiveness and readability.

## Design principles
The original Cigrid AST was designed to represent C programs with a focus on precise, unambiguous translation into low-level or assembly-like representations. In contrast, Procedural IR focuses on reconstructing the syntatic structure of C code in a structured form suitable for C code generation. To achieve this goal, several modification and simplification are introduced from the original Cigrid AST to improve expressiveness. While a pure token stream is also enough for generating C code, an AST structure has better readability and extendability for complex code structures.

It is important to note that this IR contains some syntax that are redundant from a formal C syntax perspective. For example, the distinction between `ECall` (from Cigrid) and `ECallExpr` (added), or the various assignment forms like `SVarAssign` `SArrayAssign`(from Cigrid), and `SAssign`(added), represent semantically equivalent concepts in C syntax. This AST is not suitable for a conventional C compiler because these redundancies would create serious ambiguity issues. However, since this representation serves as a tool for C code generation in a simpler manner, such duplications do not pose problems and instead provide flexibility in code construction.

Additionally, the procedural IR serves as an intermediate representation when converting from ForSyDeIR, which primarily captures ForSyDe model structure and the functions executed by actors. Consequently:

- Global definitions currently include only global function definitions.

- C constructs such as `enum`s,  `struct` definitions, `#include` directives, and `typedef`s are treated as macros for now and taken care of during the C code generation phase.

- These language elements are intentionally excluded from the current IR stage to maintain focus on the core model semantics.


## Key Components

### 1. Unary and Binary Operators
``` Haskell
data Uop
  = OPNegate        -- -rhs
  | OPLogicalNot    -- !rhs
  | OPIncrement     -- ++rhs
  | OPDecrement     -- --rhs

data Bop
data Bop
  = OPPlus            -- lhs + rhs
  | OPMinus           -- lhs - rhs
  | OPMultiply        -- lhs * rhs
  | OPDivide          -- lhs / rhs
  | OPModulo          -- lhs % rhs
  | OPPlusAssign      -- lhs += rhs
  | OPMinusAssign     -- lhs -= rhs
  | OPMultiplyAssign  -- lhs *= rhs
  | OPDivideAssign    -- lhs /= rhs
  | OPModuloAssign    -- lhs %= rhs
  | OPEqual           -- lhs == rhs  
  | OPNotEqual        -- lhs != rhs 
  | OPLogicalAnd      -- lhs && rhs
  | OPLogicalOr       -- lhs || rhs
  | OPLess            -- lhs < rhs
  | OPLessEqual       -- lhs <= rhs
  | OPGreater         -- lhs > rhs
  | OPGreaterEqual    -- lhs >= rhs
```
Compared to Cigrid, Increment(`++`), Decrement(`--`) Modulo (`%`), and compound assignment operators(`+=`,`-=`...) are added. 


Procedural IR does not contain information about the associativity of unary operators, and they are currently all treated the same way and placed at the left of the expression. This is currently fine since `++i` works in loop, but use with care if using this grammar in array access.


### 2. Types (`Type`)
```Haskell
-- Types
data Type
  = TVoid
  | TInt
  | TFloat
  | TChar
  | TIdent String
  | TPoint Type             -- int * var
  | TReference Type         -- int & var
  | TFuncPoint Type [Type]  -- void (*func)([type]).
```

C types supported by Procedural IR include:

* Primitive types (`int`, `char`,  `float`, `void`)
* Struct types (`struct Type`)
* Pointers and references (`Type*`, `Type&`)
* Function pointers (`ReturnType (*func)(ParamTypes)`)

Example usage for a function pointer type is shown in [Globals](#example-of-taking-function-pointer-as-parameter) section:


### 3. Expressions (`Expression`)

```Haskell
data Expression
  = EVar String
  | EInt Int
  | EChar Char
  | EString String
  | EBinOp Bop Expression Expression
  | EUnOp Uop Expression
  | ECall String [Expression]          
  | ECallExpr Expression [Expression] 
  | EArrayAccess Expression Expression  
  | EReference Expression             
  | EDereference Expression           
  | EMemberAccess Expression String     
  | EPointerAccess Expression String 
  | EParen expression   
```

Expressions can be considered as values, including literals, variables and its references and pointers, arithmetic, and function calls. Extended features include:

* `ECallExpr Expression [Expression]` - Extended function calls enabling calling member function of a sturct type (used with `EMemberAccess` and `EPointerAccess`) compared to normal function call (`ECall`) that only takes an string as function name. This syntax can also be considered as merely adding parenthesis and arguments after an expression (`x.foo(a)`, `x->foo(a)`).
* `EArrayAccess Expression Expression` - Modified from original Cigrid design that takes an optional string as field access. Now the syntax can be considered as merely adding brackets wrapping the second expressoin after the first expression (`expr[expr]`).
* `EReference Expression` - The address of a variable (`&x`)
* `EDereference Expression` - Dereference a pointer(`*ptr`). Note that for both reference and dereference, wrapping the expression with parenthesis will be preferable to ensure proper associativity.
* `EMemberAccess Expression String` - Struct member access (`obj.member`)
* `EPointerAccess Expression String` - Struct pointer member access (`obj->member`)
* `EParen Expression` - In traditional compilers, operator precedence is used to disambiguate and construct the correct AST structure. However, when reconstructing code from an existing AST, the reverse process may produce syntactically ambiguous or semantically incorrect expressions, such as `*a.b`. `EParen` is introduced to address this issue allowing explicit control of parenthesis when writing code templates.

* Note that `ENew` is deleted since this is a C++ keyword. In C, `malloc()` should be used instead.

### 4. Statements (`Statement`)
```Haskell
data Statement
  = SExpr Expression
  | SVarDecl Type String 
  | SVarDef Type String Expression
  | SAssign Expression Expression 
  | SVarAssign String Expression
  | SArrayDecl Type String [Expression]
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
Statements are the main building blocks of procedural program with assignments, control flow. Extended features include:

* `SVarDecl Type String` - In Cigird, it is not allowed to declare uninitialize local variables. This is extended to have local declaration (`int i;`).
* `SAssign Expression Expression` - In Cigrid, it distinguishes `SVarAssign` and `SArrayAssign` since they have different behavior in low-level codes. `SAssign` is a simpler way to represent code of assigning expression to another expression, which can be simple considered as having a `=` between these two expressions.
* `SArrayDecl Type String [Expression]` - In Cigrid, arrays can only be declared in a C++ way: `Tree* t = new Tree[1];`. In C, arrays can be declared locally as `int output[2];`. Since there can be multi-dimentional arrays `int matrix[2][2]`, the expression taken by `SArrayDecl` is a list.
* `SFor Statement Expression Statement Statement` - In Cigrid, for loops are desugared into SWhile. `SFor` is now extended, and the fields are initStatement (`int i=0`), condExpression(`i<n`), updateStatement(`++i`), and loop body. 
* `SGoto String` - Simple goto the target label.
* `SLabel String` - A statement marking the label.

* Note that `SDelete` is deleted since this is a C++ keyword. In C, `free()` should be used instead.

### 5. Globals
```Haskell
data Global
  = GFuncDef [String] Type String [(Type, String)] Statement
```

Compared to Cigrid AST, a list of String is added at the front of GFuncDef for qualifiers like `static`, `const` and `inline`.

#### Example of taking function pointer as parameter:
```c
void actor11SDF(int consum, int prod,
					 channel* ch_in, channel* ch_out,
					 void (*f) (token*, token*)){}
```
```Haskell
GFuncDef
    TVoid
    "actor11SDF"
    [
        (TInt, "consum"),
        (TInt, "prod"),
        (TPoint(TIdent "channel"), "ch_in"),
        (TPoint(TIdent "channel"), "ch_out"),
        (TFuncPoint 
        TVoid 
        [
            (TPoint (TIdent "token")), 
            (TPoint (TIdent "token"))
        ]
        ,"f")
    ]
    (
        SScope []
    )
```



### 6. Programs (`Program`)

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

- Note that in C, struct types require explicit use of the `struct` keyword (e.g., `struct fifo *buffer;`). However, if a `typedef` is used, the `struct` keyword can be omitted. The current C code implementation examples have inconsistent usage of struct types, which should be unified. Do we `typedef` in included struct types so that printing `TIdent` to C code would never need to print `struct` keyword? Or we just use `struct` keyword all the time when printing? This should be addressed in later stage but not in Procedural IR definition though.

- Printing function pointer parameters may introduce ambiguity and differs from printing other types. It is not simply a matter of printing the type followed by the variable name, as the variable name is embedded within the type syntax. For example:
    - `void *f(token*, token*)` represents a function returning a pointer.
    - `void (*f)(token*, token*)` represents a function pointer variable.
    
    Since our mainly usage of function pointer as function parameters and it is rare to use it in other places, this can also be addressed in later stage by defining an explicit template of printing parameters with type `TFuncPoint`.


