open Syntax
open Eval
open Helper
open Error
open Typing
open Builtin
open Show
open Modules
open Graph

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

let minimal = ref false
let include_path = ref ["."]

let usage = "usage: " ^ Sys.argv.(0) ^ " [--minimal]"
let speclist = [
  ("--minimal", Arg.Unit (fun () -> minimal := true), "run with minimal onscreen content");
  ("-I", Arg.String (fun s -> include_path := !include_path @ [s]), "include directory in search path")
]

let _ =
  let input () = if not !minimal then "# " else "" in
  let default_modules = add_arc "_toplevel" prelude (
    add_node "_toplevel" (add_node prelude (Graph.empty))
  ) in
  let default_env = 
    {this = "_toplevel"; modules = default_modules; namespace = builtin_fns } in
  let default_type_env = 
    {this = "_toplevel"; modules = default_modules; namespace = builtin_types } in
  let rec loop fn_env type_env =
    try 
      begin
        let lexbuf = Lexing.from_string (scan (print_string (input ()))) in
          match parse Parser.eval lexbuf with
            | INothing -> print_endline "Done"
            | ICommand com -> (match com with
                                 | "quit" -> print_endline "Done"
                                 | _ -> print_endline "Unknown command"; loop fn_env type_env
              )
            | IValue res -> 
                let (type_env', t) = type_expr type_env res in
                let (fn_env', value) = eval fn_env res
                in if not !minimal then Printf.printf ":- %s = " (print_type t) else ();
                   print_endline (show value); 
                   loop fn_env' type_env'
      end
    with exn -> handle_error exn; loop fn_env type_env
  in
    Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      usage; 
    Modules.init !include_path;
    try loop default_env default_type_env
    with exn -> handle_error exn; loop default_env default_type_env
