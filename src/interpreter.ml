open Syntax
open Eval
open Helper
open Error
open Typing
open Builtin
open Show

(* Lit une entrée à la manière d'ocaml : celle-ci doit se terminer par ';;' *)
let scan () = 
  let rec scan' n s = 
    let ns = read_line () in
    let t = String.length ns in 
    let f = try String.sub ns (t-2) 2 with _ -> "" in
      if n = 0 then begin
        if f = ";;" then s^ns
        else scan' (n+1) (s^ns)
      end
      else 
        begin 
          if f = ";;" then s^"\n"^ns
          else scan' (n+1) (s^"\n"^ns)
        end
  in scan' 0 ""

let _ =
  let rec loop fn_env type_env =
    try 
      begin
      let lexbuf = Lexing.from_string (scan (print_string "# ")) in
        match parse Parser.eval lexbuf with
          | INothing -> print_endline "Done"
          | ICommand com -> (match com with
                               | "quit" -> print_endline "Done"
                               | _ -> print_endline "Unknown command"; loop fn_env type_env
            )
          | IValue res -> 
              let (type_env', t) = type_expr type_env res in
              let (fn_env', value) = eval fn_env res
              in Printf.printf ":- %s = " (print_type t);	    
                 print_endline (show value); 
                 loop fn_env' type_env'
      end
    with exn -> handle_error exn; loop fn_env type_env
  in try loop builtin_fns builtin_types
  with exn -> handle_error exn; loop builtin_fns builtin_types
