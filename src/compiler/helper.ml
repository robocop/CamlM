(** Helper functions used throughout the compiler. *)
open Syntax
open Error

module StringSet = Set.Make(String)

(** "Increment" a string. "a" -> "b", "z" -> "aa", etc. *)
let next str = 
  let nstr = String.copy str in 
    match str.[String.length str - 1] with
      | 'z' -> nstr.[String.length str - 1] <- 'a'; nstr ^ "a"
      | c -> nstr.[String.length str - 1] <- Char.chr (Char.code c + 1);
             nstr
(** Generate a name different from any present in the string set. Used in 
    {!Lambda_repl} to generate new variables.
  *)
let rec new_variable set v = 
  if not (StringSet.mem v set) then v
  else new_variable set (next v)

(** The number 2 as a 32-bit integer. *)         
let two = Int32.of_int 2

(** 32-bit fast exponentiation function : [a^n]. *)
let rec puis a n = match n with
  | n when n=Int32.zero -> Int32.one
  | _ -> 
    let r = puis a (Int32.div n two) in
    let rr = Int32.mul r r in
    if Int32.rem n two = Int32.zero then rr else Int32.mul rr a

(** Parse and throw parse error with position in case of failure. *)
let parse f lexbuf =
  try
    f Lexer.token lexbuf
  with _ ->
    let p = Lexing.lexeme_start_p lexbuf in
    let tok = Lexing.lexeme lexbuf in
    raise (ParseError (p, tok))

(** Extract from the option type.
  
    @raise Failure if tried to extract from [None] constructor.
  *)      
let get = function
  | Some e -> e
  | None -> failwith "Tried to extract from a none value"

(** Acquire, use, release cycle. *)
let bracket acquire use release = 
  let res = acquire () in
  let ans = use res
  in release res; ans

