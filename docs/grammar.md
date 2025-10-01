Grammar
========

## Supported Haskell Features
We have simplified the Haskell language as a small core subset for our current implementation. In particular, the features we supported are: 
- **Declarations** 
    - Function declarations with explicit type signatures (```f :: Int -> Int```)
    - Function definitions (```f x = x + 1```)
    - Value bindings (```x=5```)
- **Simple Pattern Matching**
    - On function arguments (simple patterns in ```f (x,y)=...```)
    - On tuples (```(x,y)```patterns, at least 2 elements)
    - On lists (```[x,y,z]```, list must have same element type)
    - On simple literals (```x```, ```5```)
- **Types**
    - Base Type: ```Int```
    - Signal type wrapper: ```Signal Int```, ```Signal (Int, Int)```, ```Signal Signal Int``` 
    - Function types: ```x -> y```
    - Tuple types: ```(x,y)``` (at least 2 elements)
    - List types: ```[x]```
- **Expressions**
    - Identifiers and integer literals
    - Binary operations: ```+```, ```-```, ```*```
    - Unary operations: ```-```
    - Tuples and lists as values: ```(x,y)```, ```[1,2,3]```
    - Lambda functions: ```\[a] -> [a+1]```
    - Function application: ```f x```
- **ForSyDe Functions**  
    Currently, we support the following ForSyDe features: ```delaySDF```, ```actor11SDF```to```actor44SDF```. In grammar syntax definition, they are treated as identifiers in the expression, and will be used as function applications. They are considered keywords in lexing. 
 

# Grammar Syntax Definition
```
module                                                  (can ignore that)
import                                                  (can ignore that)
program             -> {declaration}                 
declaration         -> identifier :: type                                (function declaration, type signature)
                     | declaration_left "=" declaration_right                          (function definition)
types               -> type
                     | types "->" types                             (function type)
                     | "("types {"," types} ")"                     (tuple type, at least 2 elements)
                     | "[" types "]"                                (list type)
type                -> "Int" | "Signal" type                        (basic type or Signal wrapper)
declaration_right   -> expression ["where" {declaration}]
declaration_left    -> identifier                                        (value binding)
                     | identifier {pattern}                              (function definition)
                     | "(" pattern "," pattern {"," pattern} ")"    (tuple pattern binding)
                     | "["(pattern) {"," pattern}"]"                (list pattern binding)
pattern             -> identifier | int_literal
                     | "(" pattern "," pattern {"," pattern} ")"    (tuple, should be at least 2 elements)
                     | "["(pattern) {"," pattern}"]"                (list, can be empty, should have same type)
expression          -> identifier | int_literal
                     | expression binary_operation expression
                     | unary_operation expression
                     | expression expression                                    (function application)
                     | "(" expression, expression {"," expression} ")"
                     | "["(expression) {"," expression}"]"
                     | "\" pattern "->" expression                        (lambda functions)
binary_operation    -> "+" | "-" | "*"
unary_operation     -> "-"

```