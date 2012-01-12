open Syntaxe
open Eval
open Parser
open Lexer
open Lexing
open Helper


let id x = x;;
let code_nombre n = Nombre n;;
let code_bool b = Booleen b;;
let code_string s = String s;;
let decode_nombre = function
    Nombre n -> n
  | _ -> raise (Erreur "Entier attendu");;
let decode_bool = function
    Booleen b -> b
  | _ -> raise (Erreur "Bool attendu");;
let decode_string = function
  | String s -> s
  | _ -> raise (Erreur "String attendu");;


let prim2 codeur calcul decodeur =
  Primitive (fun x ->
    Primitive (fun y-> codeur (calcul (decodeur x) (decodeur y)))
  )
;;



let populateBaseScope () =
  scope :=
    [("+", prim2 code_nombre (+) decode_nombre);
     ("*", prim2 code_nombre ( * ) decode_nombre);
     ("/", prim2 code_nombre ( / ) decode_nombre);
     ("==", prim2 code_bool (=) id);
     ("!=", prim2 code_bool (<>) id);
     (">=", prim2 code_bool (>=) id);
     ("<=", prim2 code_bool (<=) id);
     ("<", prim2 code_bool (<) id);
     (">", prim2 code_bool (>) id);
     ("&&", prim2 code_bool (&&) decode_bool);
     ("||", prim2 code_bool (||) decode_bool);
     ("++", prim2 code_string ( ^ ) decode_string);
     ("not", Primitive (fun x -> code_bool (not (decode_bool x))));
     ("-", Primitive (fun x -> code_nombre (- (decode_nombre x))))
     ]

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
  | ParseError (p, tok) -> 
      print_endline ("Parse error (line " 
                     ^ string_of_int p.pos_lnum
                     ^ ", column "
                     ^ string_of_int (p.pos_cnum - p.pos_bol)
                     ^ ") on token : '"
                     ^ tok ^ "'")
  | exn -> print_endline ("Unhandled exception : " ^ Printexc.to_string exn)

let setup () = populateBaseScope ()

let _ =
  let rec loop () =
    try begin
      let lexbuf = Lexing.from_string (scan (print_string "# ")) in
        match parse Parser.eval lexbuf with
          | INothing -> print_endline "Terminé"
          | ICommand com -> (match com with
                               | "quit" -> print_endline "Terminé"
                               | _ -> print_endline "Commande inconnue"; loop ()
            )
          | IValue res -> 
              let (scope', value) = evalue !scope res
              in scope := scope'; print_endline (imprime value); 
              loop ()
    end
    with exn -> handleError exn; loop ()
  in try setup (); loop ()
  with exn -> handleError exn; loop ()
