(** Exception definitions and pretty print function.
   
    For now, most errors cause immediate termination of any eval/typing function.
    It is the responsability of the front end (CLI, REPL) to handle those errors.
  *)
open Lexing

(** Matching failed on current pattern. See the source code for {!Eval} to see
    how this is used.*)
exception MatchingFailure

(** Generic error. *)
exception Error of string

(** Parse error at specific position. *)
exception ParseError of Lexing.position * string

(** Type system caught in a loop. *)
exception Loop of string * string

(** Type conflict during unification. *)
exception Conflit of string * string

(** Multiple, equally valid names returned from an environment lookup. *)
exception MultiDef of string * string list

(** Name was not found in environment lookup. *)
exception Undef of string

(** Pretty print function. *)
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

