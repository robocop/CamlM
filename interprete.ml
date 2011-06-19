open Syntaxe
(* Exemple : définition de fac et calcul de fac 5*)
(* n-1 *)

let nminus1 = Application (Application(Variable "-", Variable "n"), Nombre 1);;
(* function 0 -> 1 | n -> n * fac(n-1) *)
let expression = Fonction(
  [(Motif_nombre 0, Nombre 1); 
   (Motif_variable "n", 
    Application(Application(Variable "*", Application(Variable "fac", nminus1)), Variable "n"))
]);;

(* let rec fac n = function | 0 -> 1 | n -> n*fac(n-1) in fac 5*)
let expr = (Let  ({ recursive = true; nom = "fac"; expr = expression}, Application(Variable "fac", Nombre 5)));;


(* let rec fac n = function | 0 -> 1 | n -> n*fac(n-1);;*)

let fac = ({recursive = true; nom = "fac"; expr = expression})

;;



let code_nombre n = Val_nombre n;;
let decode_nombre = function Val_nombre n -> n | _ -> raise (Erreur "entier attendu");;
let prim2 codeur calcul decodeur = 
  Val_primitive (fun x -> 
    Val_primitive (fun y-> codeur (calcul (decodeur x) (decodeur y)))
  )

let env_initial = 
[("-", prim2 code_nombre (-) decode_nombre); 
 ("*", prim2 code_nombre ( * ) decode_nombre)
];;

let _  = 
  print_int (decode_nombre (evalue env_initial expr));;
