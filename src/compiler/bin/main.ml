open Compiler

let () =
  let lexbuf = Lexing.from_channel stdin in
  Parser.main Lexer.token lexbuf
