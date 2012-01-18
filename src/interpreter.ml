open Syntax
open Eval
open Helper
open Error
open Typing
open Builtin
open Show

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
  let rec loop env =
    try 
      begin
      let lexbuf = Lexing.from_string (scan (print_string "# ")) in
        match parse Parser.eval lexbuf with
          | INothing -> print_endline "Done"
          | ICommand com -> (match com with
                               | "quit" -> print_endline "Done"
                               | _ -> print_endline "Unknown command"; loop env
            )
          | IValue res -> 
	    
	    let (scope_t', t) = type_expr (builtin_types @ !type_scope) res in
        let (env', value) = eval env res
        in Printf.printf ":- %s = \n" (print_type t);	    
           print_endline (show value); 
           type_scope := scope_t'; 
           loop (env' @ env)
      end
    with exn -> handle_error exn; loop env
  in try loop builtin_fns
  with exn -> handle_error exn; loop builtin_fns
