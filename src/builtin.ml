open Syntax
open Error
open Typing
open Eval

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

let type_prim1 a b = trivial_schema (type_arrow a b)
let type_prim2 a b c = 
  trivial_schema (type_arrow a (type_arrow b c))
  
let type_arithmetic = type_prim2 type_int type_int type_int
let type_logic = type_prim2 type_bool type_bool type_bool
let type_poly_logic = let v = new_unknow () in type_prim2 v v type_bool


let builtin_types = 
  [("+", type_arithmetic);
   ("*", type_arithmetic);
   ("/", type_arithmetic);
   ("==", type_poly_logic);
   ("!=", type_poly_logic);
   (">=", type_poly_logic);
   ("<=", type_poly_logic);
   ("||", type_logic);
   ("&&", type_logic);
   ("not", type_prim1 type_bool type_bool);
   ("-", type_prim1 type_int type_int)
   ]

let builtin_fns = 
   [("+", prim2 to_num (+) from_num);
   ("*", prim2 to_num ( * ) from_num);
   ("/", prim2 to_num ( / ) from_num);
   ("==", prim2 to_bool (=) id);
   ("!=", prim2 to_bool (<>) id);
   (">=", prim2 to_bool (>=) id);
   ("<=", prim2 to_bool (<=) id);
   ("<",  prim2 to_bool (<) id);
   (">",  prim2 to_bool (>) id);
   ("&&", prim2 to_bool (&&) from_bool);
   ("||", prim2 to_bool (||) from_bool);
   ("++", prim2 to_string ( ^ ) from_string);
   ("not", EPrimitive (fun x -> to_bool (not (from_bool x))));
   ("-", EPrimitive (fun x -> to_num (- (from_num x))))
  ]

