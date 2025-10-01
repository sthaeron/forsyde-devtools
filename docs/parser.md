# Parser Documentation

## Naming Conventions
The production rules should match with the concrete syntax in the grammar specification.
The abstract syntax tree returned by the parser should match with the abstract syntax in the
grammar specification.
The pretty printing should match with what is defined in the grammar specification.
If anything is missing, they should be added to the specification before and comply with
the following naming conventions.

Abbreviations should be avoided.

### Concrete Syntax

Production rules should be named with lower camel case.
Non-terminals which are not production rules should be upper camel case.

### Abstract Syntax

The rules in the abstract syntax should be written with lower camel case,
which will correspond to the OCaml variant type in the implementation.
The right hand side should be written as upper camel case, which will correspond
to the OCaml constructors in the implementation.

#### Pretty Printing

The pretty printing should closely match with the abstract syntax, with some differences.
- Text inside "" quotes are terminal symbols, i.e. what will actually be printed
- Optional values are instead enclosed in [] brackets
- List values are instead enclosed in {} brackets

## Menhir

When run with the `--explain` flag which we have specified in our build, menhir
will generate a description of all shift/reduce confilcts in the file
`_build/default/lib/parser.conflicts`
