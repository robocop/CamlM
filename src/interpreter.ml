open Syntax
open Eval
open Helper
open Error
open Typing
open Show

let id x = x
let to_num n = ENum n
let to_bool b = EBoolean b
let to_string s = EString s
let from_num = function
    ENum n -> n
  | _ -> raise (Error "Expecting integer")
let from_bool = function
    EBoolean b -> b
  | _ -> raise (Error "Expecting boolean")
let from_string = function
  | EString s -> s
  | _ -> raise (Error "Expecting string")

let prim2 encoder computation decoder =
  EPrimitive (fun x ->
    EPrimitive (fun y -> encoder (computation (decoder x) (decoder y)))
  )

let type_arithmetic = trivial_schema
  (type_arrow (type_arrow type_int type_int) type_int)

let populate_base_scope () =
  scope :=
  [("+", prim2 to_num (+) from_num);
   ("*", prim2 to_num ( * ) from_num);
   ("/", prim2 to_num ( / ) from_num);
   ("==", prim2 to_bool (=) id);
   ("!=", prim2 to_bool (<>) id);
   (">=", prim2 to_bool (>=) id);
   ("<=", prim2 to_bool (<=) id);
   ("<", prim2 to_bool (<) id);
   (">", prim2 to_bool (>) id);
   ("&&", prim2 to_bool (&&) from_bool);
   ("||", prim2 to_bool (||) from_bool);
   ("++", prim2 to_string ( ^ ) from_string);
   ("not", EPrimitive (fun x -> to_bool (not (from_bool x))));
   ("-", EPrimitive (fun x -> to_num (- (from_num x))))
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

let _ =
  let rec loop () =
    try 
      begin
      let lexbuf = Lexing.from_string (scan (print_string "# ")) in
        match parse Parser.eval lexbuf with
          | INothing -> print_endline "Done"
          | ICommand com -> (match com with
                               | "quit" -> print_endline "Done"
                               | _ -> print_endline "Unknown command"; loop ()
            )
          | IValue res -> 
            let (scope', value) = eval !scope res
            in scope := scope'; print_endline (show value); 
            loop ()
      end
    with exn -> handle_error exn; loop ()
  in try populate_base_scope (); loop ()
  with exn -> handle_error exn; loop ()
