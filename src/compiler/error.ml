open Lexing

exception MatchingFailure
exception Error of string
exception ParseError of Lexing.position * string
exception Loop of string * string
exception Conflit of string * string
exception MultiDef of string * string list
exception Undef of string

let handle_error = function
  | Error s -> print_endline ("Error : " ^ s) 
  | Undef f -> print_endline ("Error : " ^ f ^ " is not defined")
  | MultiDef (f, mods) -> 
      let rec pretty_error = function
        | [] -> ()
        | m :: xs -> 
            print_endline (" - " ^ f ^ " in module " ^ m);
            pretty_error xs
      in print_endline ("Error : multiple definitions of " ^ f ^ ", did you mean: ");
         pretty_error mods
  | ParseError (p, tok) -> 
      print_endline ("Parse error (line " 
                     ^ string_of_int p.pos_lnum
                     ^ ", column "
                     ^ string_of_int (p.pos_cnum - p.pos_bol)
                     ^ ") on token : '"
                     ^ tok ^ "'")
  | exn -> print_endline ("Unhandled exception : " ^ Printexc.to_string exn)

