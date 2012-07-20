(* Définition de la représentation interne d'une code CamlM *)

type expression = 
  | EVariable of string
  | EFunction of closure
  | EApplication of expression * expression
  | ELet of definition * expression option
  | EDeclare of string * expression option
  | EOpen of string * expression option
  | EBoolean of bool
  | ENum of int32
  | EPair of expression * expression
  | EUnit
  | ENil
  | ECons of expression * expression
  | ENone  
  | EString of string
  | ESome of expression

and closure = 
    { def : (pattern * expression) list; 
	    mutable env : env option }

(* In order : 
 *  - name (e.g. "+", "foo")
 *  - value
 *  - operator properties
 *    - if the element is not an operator, then None
 *    - otherwise, Some properties
 *)
and env_content = string * (expression option) * ((prop list) option)

(* this : name of this module
 * anon_modules : modules that can be accessed without prefixing the expression
 *    with the module name : 
 *    a.ml : let bob () = ...;
 *     - anon module : bob ()
 *     - qualified module : A.bob ()
 * modules : list of module names and content
 *)

and env = {
  this: string;
  anon_modules: string list;
  modules: (string * env_content list) list
}

and prop = 
  | Com
  | Assoc

and pattern = 
  | PAll
  | PVariable of string
  | PAxiom of string
  | PBoolean of bool
  | PNum of int32
  | PPair of pattern * pattern
  | PNil
  | PCons of pattern * pattern
  | PNone
  | PSome of pattern 
  | PString of string
  | POp of string * pattern * pattern
  | PMinus of pattern
  | PCompose of pattern * pattern
  | PIdentity
  | PConst of pattern
  | PIsnum of pattern
  | PWhen of expression * pattern

and definition = 
    {
      recursive:bool;
      name:string;
      expr:expression
    }
;;

type sentence =
  | Expression of expression
  | Definition of definition
;;

type 'a interpreter = 
  | INothing                 (* Rien n'a ete entre *)
  | ICommand of string       (* Une commande de l'interpreteur *)
  | IValue of 'a             (* Une expression a evaluer *)

