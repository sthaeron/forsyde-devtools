# Lexer Documentation

## Token Naming Conventions

The purpose of the naming conventions is to reduce any conflicts
arising from parallel work on different features.

- Tokens should not be abbreviated
- Tokens should be written in upper case
- Structural tokens should be nouns (e.g. LEFTBRACKET)
- Operator tokens:
    - Arithmetic operators should be verbs (e.g. ADD, SHIFTLEFT, NEGATE)
    - Directional comparison operators should be two components, where the first specifies
       the direction of the comparison and the second indicates if it's inclusive or not
       (e.g. LESSTHAN or LESSEQUAL)
- Keyword tokens should have the same name as the keyword but upper case
- Process constructor tokens have the same name as in the ForSyDe shallow project but upper case (e.g. DELAYSDF, ACTOR11SDF)

## Ocamllex

The Ocamllex reference can be found [here](https://ocaml.org/manual/5.3/api/Lexing.html)
and a a description on how to used [here](https://ocaml.org/manual/5.3/lexyacc.html)
