open Syntax
open Error

let parse f lexbuf =
  try
    f Lexer.token lexbuf
  with _ ->
    let p = Lexing.lexeme_start_p lexbuf in
    let tok = Lexing.lexeme lexbuf in
    raise (ParseError (p, tok))

