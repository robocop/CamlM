type expression = 
  | Variable of string
  | Fonction of fermeture
  | Primitive of string * (expression -> expression)
  | Application of expression * expression
  | Let of definition * expression option
  | Booleen of bool
  | Nombre of int
  | Paire of expression * expression
  | Nil
  | Cons of expression * expression
  | CNone  
  | String of string
  | CSome of expression

and fermeture = 
      { def : (motif * expression) list; 
	mutable environnement : environnement option }
and environnement = (string * expression) list

and motif = 
  | Motif_all
  | Motif_when of expression * motif
  | Motif_variable of string
  | Motif_booleen of bool
  | Motif_nombre of int
  | Motif_paire of motif * motif
  | Motif_nil
  | Motif_cons of motif * motif
  | Motif_none
  | Motif_some of motif 
  | Motif_string of string
  | FMotif_op of string*motif * motif
  | FMotif_Id
  | FMotif_const of motif

and definition = 
    {
      recursive:bool;
      nom:string;
      expr:expression
    }
;;

type phrase =
  | Expression of expression
  | Definition of definition
;;

type 'a interpreter = 
  | INothing                 (* Rien n'a ete entre *)
  | ICommand of string       (* Une commande de l'interpreteur *)
  | IValue of 'a             (* Une expression a evaluer *)

exception ParseError of Lexing.position * string;;
