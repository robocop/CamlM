open Syntaxe
open Eval
open Parser
open Lexer
open Formel

let code_nombre n = Val_nombre n;;
let decode_nombre = function Val_nombre n -> n | _ -> raise (Erreur "entier attendu");;

let prim2 codeur calcul decodeur = 
  Val_primitive (fun x -> 
    Val_primitive (fun y-> codeur (calcul (decodeur x) (decodeur y)))
  )

let env_initial = 
[("+", prim2 code_nombre (+) decode_nombre); 
 ("*", prim2 code_nombre ( * ) decode_nombre);
 ("-", prim2 code_nombre (-) decode_nombre);
 ("add", primitive_add);
 ("mult", primitive_mult);
 ("compose", primitive_compose);
 ("const", primitive_const);
 ("id", primitive_id)
];;

let parse expr = Parser.eval Lexer.token (Lexing.from_string expr);;

parse "1::[]";;
let scan () = 
  let rec scan' n s = 
    let ns = read_line () in
    let t = String.length ns in 
    let f = try String.sub ns (t-2) 2 with _ -> "" in
    if n = 0 then begin
      if f = ";;" then s^(String.sub ns 0 (t-2))
      else scan' (n+1) (s^ns)
    end
    else 
      begin 
        if f = ";;" then s^"\n"^(String.sub ns 0 (t-2))
        else scan' (n+1) (s^"\n"^ns)
      end
  in scan' 0 ""
;;


let _ =
  let rec loop () =
    try 
      (
	let expr = scan (print_string "# ") in
	match parse expr with
	  | None -> print_endline "Terminé"
	  | Some res -> print_endline (imprime_valeur (evalue env_initial res)); print_newline (); loop ()
      )
    with Erreur s -> print_endline ("erreur : " ^ s);
      loop()
  in
loop ()
