open Syntax
open Error
open Typing
open Eval

let type_prim1 a b = (trivial_schema (type_arrow a b), false)
let type_prim2 a b c = 
  (trivial_schema (type_arrow a (type_arrow b c)), false)
  
let type_arithmetic = type_prim2 type_int type_int type_int
let type_lexical = type_prim2 type_string type_string type_string
let type_logic = type_prim2 type_bool type_bool type_bool
let type_poly_logic = let v = new_unknow () in type_prim2 v v type_bool


let builtin_types = 
  [("+", type_arithmetic);
   ("*", type_arithmetic);
   ("/", type_arithmetic);
   ("^", type_arithmetic);
   ("==", type_poly_logic);
   ("!=", type_poly_logic);
   (">=", type_poly_logic);
   ("<=", type_poly_logic);
   ("||", type_logic);
   ("&&", type_logic);
   ("not", type_prim1 type_bool type_bool);
   ("-", type_prim1 type_int type_int);
   ("++", type_lexical);
   ("mod", type_arithmetic);
   ("string_of_int", type_prim1 type_int type_string)
   ]

let builtin_fns = 
   [("+", Some (EVariable "+"));
   ("*", Some (EVariable "*"));
   ("/", Some (EVariable "/"));
   ("^", Some (EVariable "^"));
   ("==", Some (EVariable "=="));
   ("!=", Some (EVariable "!="));
   (">=", Some (EVariable ">="));
   ("<=", Some (EVariable "<="));
   ("<",  Some (EVariable "<"));
   (">",  Some (EVariable ">"));
   ("&&", Some (EVariable "&&"));
   ("||", Some (EVariable "||"));
   ("++", Some (EVariable "++"));
   ("not", Some (EVariable "not"));
   ("-", Some (EVariable "-"));
   ("++", Some (EVariable "++"));
   ("mod", Some (EVariable "mod"));
   ("string_of_int", Some (EVariable "string_of_int"))
  ]
