open Syntax
open Error
open Typing
open Eval
open Modules

(* Ce fichier contient les définitions (defs et types) des opérateurs et fonctions par défaut. *)
(* La façon dont sont simplifiés les calculs utilisant ces définitions                         *)
(*  (par exemple l'addition de deux nombres) est définie dans eval.ml                          *)
(* Les opérateurs '+' et '*' sont définis comme commutatifs et associatifs                     *)
let type_prim1 a b = (trivial_schema (type_arrow a b), false)
let type_prim2 a b c = 
  (trivial_schema (type_arrow a (type_arrow b c)), false)
  
let type_arithmetic = type_prim2 type_int type_int type_int
let type_lexical = type_prim2 type_string type_string type_string
let type_logic = type_prim2 type_bool type_bool type_bool
let type_poly_logic = let v = new_unknow () in type_prim2 v v type_bool

let builtin_types =
  [("+", [prelude, type_arithmetic]);
   ("*", [prelude, type_arithmetic]);
   ("/", [prelude, type_arithmetic]);
   ("^", [prelude, type_arithmetic]);
   ("==", [prelude, type_poly_logic]);
   ("!=", [prelude, type_poly_logic]);
   (">=", [prelude, type_poly_logic]);
   ("<=", [prelude, type_poly_logic]);
   ("||", [prelude, type_logic]);
   ("&&", [prelude, type_logic]);
   ("not", [prelude, type_prim1 type_bool type_bool]);
   ("-", [prelude, type_prim1 type_int type_int]);
   ("++", [prelude, type_lexical]);
   ("mod", [prelude, type_arithmetic]);
   ("string_of_int", [prelude, type_prim1 type_int type_string])
   ]

let builtin_fns = 
   [("+", [prelude, (Some (EVariable "+"), Some [Assoc; Com])]);
   ("*", [prelude, (Some (EVariable "*"), Some [Assoc; Com])]);
   ("/", [prelude, (Some (EVariable "/"), Some [])]);
   ("^", [prelude, (Some (EVariable "^"), Some [])]);
   ("==", [prelude, (Some (EVariable "=="), None)]);
   ("!=", [prelude, (Some (EVariable "!="), None)]);
   (">=", [prelude, (Some (EVariable ">="), None)]);
   ("<=", [prelude, (Some (EVariable "<="), None)]);
   ("<",  [prelude, (Some (EVariable "<"), None)]);
   (">",  [prelude, (Some (EVariable ">"), None)]);
   ("&&", [prelude, (Some (EVariable "&&"), None)]);
   ("||", [prelude, (Some (EVariable "||"), None)]);
   ("++", [prelude, (Some (EVariable "++"), None)]);
   ("not", [prelude, (Some (EVariable "not"), None)]);
   ("-", [prelude, (Some (EVariable "-"), None)]);
   ("++", [prelude, (Some (EVariable "++"), None)]);
   ("mod", [prelude, (Some (EVariable "mod"), None)]);
   ("string_of_int", [prelude, (Some (EVariable "string_of_int"), None)])
  ]

