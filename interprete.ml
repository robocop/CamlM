open Syntaxe
open Eval
open Parser
open Lexer
open Lexing

let code_nombre n = Val_nombre n;;
let decode_nombre = function Val_nombre n -> n | _ -> raise (Erreur "entier attendu");;
let prim2 codeur calcul decodeur = 
  Val_primitive (fun x -> 
    Val_primitive (fun y-> codeur (calcul (decodeur x) (decodeur y)))
  )

let env_initial scope = 
scope @
[("+", prim2 code_nombre (+) decode_nombre); 
 ("*", prim2 code_nombre ( * ) decode_nombre);
 ("-", prim2 code_nombre (-) decode_nombre);
];;

let parse expr = Parser.eval Lexer.token (Lexing.from_string expr);;

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
;;

let handleError = function
  | Erreur s -> print_endline ("Erreur : " ^ s)
  | ParseError p -> print_endline ("Parse error : ligne " 
                                   ^ string_of_int p.pos_lnum
                                   ^ ", character "
                                   ^ string_of_int p.pos_cnum)
  | _ -> print_endline "Unhandle exception"

let _ =
  let rec loop () =
    try 
      (
	    let expr = scan (print_string "# ") in
	    match parse expr with
	      | None -> print_endline "Terminé"
	      | Some res -> print_endline (imprime_valeur (evalue (env_initial !scope) res)); print_newline (); loop ()
      )
    with exn -> handleError exn; loop()
  in
loop ()
