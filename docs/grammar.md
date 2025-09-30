Grammar
========

# Grammar Syntax Definition
```
module                                                  (can ignore that)
import                                                  (can ignore that)
program -> {decl}                 
decl    -> ident :: type                                (function declaration, type signature, get rid of "context" from Haskell)
         | decllhs "=" declrhs                          (function definition)
types   -> type
         | types "->" types                             (function type)
         | "("types {"," types} ")"                     (tuple type, at least 2 elements)
         | "[" types "]"                                (list type)
type    -> "Int" | "Signal" type                        (basic type or Signal wrapper)
declrhs -> expr ["where" {decl}]
decllhs -> ident                                        (value binding)
         | ident {pattern}                              (function definition)
         | "(" pattern "," pattern {"," pattern} ")"    (tuple pattern binding)
         | "["(pattern) {"," pattern}"]"                (list pattern binding)
pattern -> ident | intlit
         | "(" pattern "," pattern {"," pattern} ")"    (tuple, should be at least 2 elements)
         | "["(pattern) {"," pattern}"]"                (list, can be empty, should have same type)
expr    -> ident | intlit
         | expr binop expr
         | unop expr
         | expr expr                                    (function application)
         | "(" expr, expr {"," expr} ")"
         | "["(expr) {"," expr}"]"
         | "\" pattern "->" expr                        (lambda functions)

```
# Grammar Semantics (Abstract Syntax)