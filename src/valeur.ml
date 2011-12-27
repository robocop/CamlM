open Syntaxe 

type valeur = 
  | Val_nombre of int
  | Val_string of string
  | Val_booleenne of bool
  | Val_paire of valeur * valeur
  | Val_nuple of valeur list
  | Val_nil
  | Val_cons of valeur * valeur
  | Val_fermeture of fermeture
  | Val_primitive of (valeur -> valeur)
  | Val_some of valeur
  | Val_none 

and fermeture = 
    { definition: ((motif * expression) list); mutable environnement: environnement }
and environnement = (string * valeur) list;;

exception Echec_filtrage;;
exception Erreur of string;;

