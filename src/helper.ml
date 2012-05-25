open Syntax
open Error

module StringSet = Set.Make(String)

let next str = 
  let nstr = String.copy str in 
    match str.[String.length str - 1] with
      | 'z' -> nstr.[String.length str - 1] <- 'a'; nstr ^ "a"
      | c -> nstr.[String.length str - 1] <- Char.chr (Char.code c + 1);
             nstr

let rec new_variable set v = 
  if not (StringSet.mem v set) then v
  else new_variable set (next v)

let rec puis a n = match n with
  | 0 -> 1
  | n -> 
    let r = puis a (n/2) in
    if n mod 2 = 0 then r*r else r*r*a

let parse f lexbuf =
  try
    f Lexer.token lexbuf
  with _ ->
    let p = Lexing.lexeme_start_p lexbuf in
    let tok = Lexing.lexeme lexbuf in
    raise (ParseError (p, tok))

