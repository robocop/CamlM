type expression = 
  | EVariable of string
  | EFunction of closure
  | EApplication of expression * expression
  | ELet of definition * expression option
  | EOpen of string * expression option
  | EBoolean of bool
  | ENum of int
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
and env = (string * expression) list

and pattern = 
  | PAll
  | PWhen of expression * pattern
  | PVariable of string
  | PBoolean of bool
  | PNum of int
  | PPair of pattern * pattern
  | PNil
  | PCons of pattern * pattern
  | PNone
  | PSome of pattern 
  | PString of string
  | POp of string * pattern * pattern
  | PMinus of pattern
  | PApplication of pattern * pattern

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

