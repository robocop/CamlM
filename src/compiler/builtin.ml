(** Default (prelude) function types and definitions.
   
    {!Eval} dictates the reduction rules for Prelude functions.
    The builtin functions below are mixed in with the functions from the
    "prelude.mml" file in the lib/ directory by the REPL.
  *)
open Syntax
open Error
open Typing
open Eval
open Modules

let type_prim1 a b =  trivial_schema (type_arrow a b)
let type_prim2 a b c = 
   trivial_schema (type_arrow a (type_arrow b c))
  
let type_arithmetic = type_prim2 type_num type_num type_num
let type_lexical = type_prim2 type_string type_string type_string
let type_logic = type_prim2 type_bool type_bool type_bool
let type_poly_logic = let v = new_unknow () in type_prim2 v v type_bool


(** Builtin types. Note that {!builtin_types} is structured to be a namespace
    for {!Syntax.env}.
  *)
let builtin_types =
  [("+", [prelude, type_arithmetic]);
   ("*", [prelude, type_arithmetic]);
   ("/", [prelude, type_arithmetic]);
   ("^", [prelude, type_arithmetic]);
   ("==", [prelude, type_poly_logic]);
   ("!=", [prelude, type_poly_logic]);
   (">=", [prelude, type_poly_logic]);
   ("<=", [prelude, type_poly_logic]);
   (">", [prelude, type_poly_logic]);
   ("<", [prelude, type_poly_logic]);
   ("||", [prelude, type_logic]);
   ("&&", [prelude, type_logic]);
   ("not", [prelude, type_prim1 type_bool type_bool]);
   ("-", [prelude, type_prim1 type_num type_num]);
   ("++", [prelude, type_lexical]);
   ("mod", [prelude, type_arithmetic]);
   ("string_of_int", [prelude, type_prim1 type_num type_string])
   ]

(** Builtin functions and their representation and properties in the AST.
    [+] and [-] are defined to be associative and commutative. Like 
    {!builtin_types}, {!builtin_fns} is structured to be a namespace for
    {!Syntax.env}.
*)
let builtin_fns = 
   [("+", [prelude, (EVariable "+", [Assoc; Com])]);
   ("*", [prelude, (EVariable "*", [Assoc; Com])]);
   ("/", [prelude, (EVariable "/", [])]);
   ("^", [prelude, (EVariable "^", [])]);
   ("==", [prelude, (EVariable "==", [])]);
   ("!=", [prelude, (EVariable "!=", [])]);
   (">=", [prelude, (EVariable ">=", [])]);
   ("<=", [prelude, (EVariable "<=", [])]);
   ("<",  [prelude, (EVariable "<", [])]);
   (">",  [prelude, (EVariable ">", [])]);
   ("&&", [prelude, (EVariable "&&", [])]);
   ("||", [prelude, (EVariable "||", [])]);
   ("not", [prelude, (EVariable "not", [])]);
   ("-", [prelude, (EVariable "-", [])]);
   ("++", [prelude, (EVariable "++", [])]);
   ("mod", [prelude, (EVariable "mod", [])]);
   ("string_of_int", [prelude, (EVariable "string_of_int", [])]);
  ]

