open Lexing

exception MatchingFailure
exception Error of string
exception ParseError of Lexing.position * string;;

let handle_error = function
  | Error s -> print_endline ("Error : " ^ s) 
  | ParseError (p, tok) -> 
      print_endline ("Parse error (line " 
                     ^ string_of_int p.pos_lnum
                     ^ ", column "
                     ^ string_of_int (p.pos_cnum - p.pos_bol)
                     ^ ") on token : '"
                     ^ tok ^ "'")
  | exn -> print_endline ("Unhandled exception : " ^ Printexc.to_string exn)

