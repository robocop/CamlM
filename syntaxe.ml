type expression = 
  | Variable of string
  | Fonction of (motif * expression) list
  | Application of expression * expression
  | Let of definition * expression option
  | Booleen of bool
  | Nombre of int
  | Paire of expression * expression
  | Nil
  | Cons of expression * expression
  | CNone  
  | CSome of expression
  
and motif = 
  | Motif_all
  | Motif_variable of string
  | Motif_booleen of bool
  | Motif_nombre of int
  | Motif_paire of motif * motif
  | Motif_nil
  | Motif_cons of motif * motif
  | Motif_none
  | Motif_some of motif 
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

exception ParseError of Lexing.position;;
