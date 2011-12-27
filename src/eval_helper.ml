open Syntaxe
open Valeur

(* zip2 <=> [(a, b) | a <- l1_orig; b <- l2_orig] *)
let zip2 l1_orig l2_orig = 
  let rec aux l1 l2 = match l1 with
    | Val_nil -> Val_nil
    | Val_cons (a, l1tl) -> ( match l2 with
        | Val_nil -> aux l1tl l2_orig
        | Val_cons (b, l2tl) -> Val_cons (Val_paire (a, b), 
                                          aux (Val_cons (a, l1tl)) l2tl) 
        | _ -> raise (Erreur "Cannot zip non-list values")
      )
    | _ -> raise (Erreur "Cannot zip non-list values")
  in aux l1_orig l2_orig

(* Pour une liste de listes l1, l2, ..., ln, zipn genere :
    [(v1, (v2, (..., vn))) | v1 <- l1; v2 <- l2; ...; vn <- ln]
 *)
let rec zipn = function
  | [l1] -> l1
  | l :: ls -> zip2 l (zipn ls)
  | _ -> raise (Erreur "Cannot zip non-list values")

(* (a, (b, (..., c))) => (a, b, ..., c) *)
let flattenPair p = 
  let rec aux = function
    | Val_paire (a, b) -> a :: (aux b)
    | a -> [a]
  in Val_nuple (aux p) 

(* List.map sur des listes de valeurs *)
let rec valMap f = function
  | Val_nil -> Val_nil
  | Val_cons (x, xs) -> Val_cons (f x, valMap f xs)
  | _ -> raise (Erreur "Cannot map on a non list value")

let fst = function (x, _) -> x;;
let snd = function (_, y) -> y;;

