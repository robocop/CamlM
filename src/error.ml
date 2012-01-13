open Lexing

exception Echec_filtrage
exception Erreur of string
exception ParseError of Lexing.position * string;;

let handleError = function
  | Erreur s -> print_endline ("Erreur : " ^ s) 
  | ParseError (p, tok) -> 
      print_endline ("Parse error (line " 
                     ^ string_of_int p.pos_lnum
                     ^ ", column "
                     ^ string_of_int (p.pos_cnum - p.pos_bol)
                     ^ ") on token : '"
                     ^ tok ^ "'")
  | exn -> print_endline ("Unhandled exception : " ^ Printexc.to_string exn)

