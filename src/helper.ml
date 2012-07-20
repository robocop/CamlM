open Syntax
open Error

module StringSet = Set.Make(String)

let next str = 
  let nstr = String.copy str in 
    match str.[String.length str - 1] with
      | 'z' -> nstr.[String.length str - 1] <- 'a'; nstr ^ "a"
      | c -> nstr.[String.length str - 1] <- Char.chr (Char.code c + 1);
             nstr
(* Sert à générer un nom pour une nouvelle variable différent de ceux présents dans set *)
(* Il est parfois necessaire de générer des nouvelles variables, notamment quand on
  substitue une expression dans une autre, cf lamda_repl.ml                             *)
let rec new_variable set v = 
  if not (StringSet.mem v set) then v
  else new_variable set (next v)

let two = Int32.of_int 2

let rec puis a n = match n with
  | n when n=Int32.zero -> Int32.one
  | _ -> 
    let r = puis a (Int32.div n two) in
    let rr = Int32.mul r r in
    if Int32.rem n two = Int32.zero then rr else Int32.mul rr a

let parse f lexbuf =
  try
    f Lexer.token lexbuf
  with _ ->
    let p = Lexing.lexeme_start_p lexbuf in
    let tok = Lexing.lexeme lexbuf in
    raise (ParseError (p, tok))


let get = function
  | Some e -> e
  | None -> failwith "Tried to extract from a none value"
