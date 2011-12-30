open Syntaxe
open Eval
open Parser
open Lexer
open Lexing

let parse f lexbuf =
  try
    f Lexer.token lexbuf
  with _ ->
    let p = Lexing.lexeme_start_p lexbuf in
    let tok = Lexing.lexeme lexbuf in
      raise (ParseError (p, tok))

let code_nombre n = Nombre n;;
let decode_nombre = function
    Nombre n -> n
  | _ -> raise (Erreur "Entier attendu");;

let prim2 nom codeur calcul decodeur =
  Primitive (nom, (fun x ->
    Primitive (nom, (fun y-> codeur (calcul (decodeur x) (decodeur y))))
  ))
;;



let populateBaseScope () =
  scope :=
    [("+", prim2 "+" code_nombre (+) decode_nombre);
     ("-", prim2 "-" code_nombre (-) decode_nombre);
     ("*", prim2 "*" code_nombre ( * ) decode_nombre);
     ("/", prim2 "/" code_nombre ( / ) decode_nombre);
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

let rec doEval fname = function
  | [] -> print_endline (fname ^ " loaded.\n")
  | x :: xs -> 
      print_endline (imprime(evalue !scope x)); 
      doEval fname xs


let rec loadFiles = function
  | [] -> ()
  | x :: xs -> 
      print_endline ("Loading file : " ^ x);
      let handle = open_in x in
      let ast = parse Parser.file (Lexing.from_channel handle)
      in doEval x ast; close_in handle; loadFiles xs

let setup () = 
  let parseList = ref [] in
    begin
      populateBaseScope ();
      Arg.parse [] (fun x -> parseList := !parseList @ [x]) "filename(s)";
      loadFiles !parseList
    end

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
              print_endline (imprime (evalue !scope res)); 
              loop ()
    end
    with exn -> handleError exn; loop ()
  in try setup (); loop ()
  with exn -> handleError exn; loop ()
