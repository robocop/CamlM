open Syntax
open Eval
open Parser
open Lexer
open Lexing
open Helper
open Error
open Show

let id x = x
let code_nombre n = ENum n
let code_bool b = EBoolean b
let code_string s = EString s
let decode_nombre = function
    ENum n -> n
  | _ -> raise (Error "Expecting integer")
let decode_bool = function
    EBoolean b -> b
  | _ -> raise (Error "Expecting boolean")
let decode_string = function
  | EString s -> s
  | _ -> raise (Error "Expecting string")

let prim2 codeur calcul decodeur =
  EPrimitive (fun x ->
               EPrimitive (fun y-> codeur (calcul (decodeur x) (decodeur y)))
  )

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
   ("not", EPrimitive (fun x -> code_bool (not (decode_bool x))));
   ("-", EPrimitive (fun x -> code_nombre (- (decode_nombre x))))
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

let setup () = populateBaseScope ()

let _ =
  let rec loop () =
    try begin
      let lexbuf = Lexing.from_string (scan (print_string "# ")) in
        match parse Parser.eval lexbuf with
          | INothing -> print_endline "Done"
          | ICommand com -> (match com with
                               | "quit" -> print_endline "Done"
                               | _ -> print_endline "Unknown command"; loop ()
            )
          | IValue res -> 
              let (scope', value) = evalue !scope res
              in scope := scope'; print_endline (imprime value); 
                 loop ()
    end
    with exn -> handleError exn; loop ()
  in try setup (); loop ()
  with exn -> handleError exn; loop ()
