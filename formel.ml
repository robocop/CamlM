open Syntaxe
open Eval


let new_def_plus def = match def with
  | [Motif_variable v, expr] ->
    (match expr with
      | Application (Application (Variable "+", e1), e2) ->
	let d1, d2 = [Motif_variable v, e1],  [Motif_variable v, e2] in
	Some (d1, d2)
      | _ -> None)
  | _ -> None
let primitive_add = Val_primitive 
  (fun f -> match f with 
    | Val_fermeture {definition=def; environnement = env } -> 
      (match new_def_plus def with
	| Some (d1, d2) ->
	  let make d =  Val_fermeture{definition=d; environnement = env } in
	  Val_some (Val_paire(make d1, make d2))
	| None -> Val_none)
    | _ -> Val_none
  )

let new_def_mult def = match def with
  | [Motif_variable v, expr] ->
    (match expr with
      | Application (Application (Variable "*", e1), e2) ->
	let d1, d2 = [Motif_variable v, e1],  [Motif_variable v, e2] in
	Some (d1, d2)
      | _ -> None)
  | _ -> None
let primitive_mult = Val_primitive 
  (fun f -> match f with 
    | Val_fermeture {definition=def; environnement = env } -> 
      (match new_def_mult def with
	| Some (d1, d2) ->
	  let make d =  Val_fermeture{definition=d; environnement = env } in
	  Val_some (Val_paire(make d1, make d2))
	| None -> Val_none)
    | _ -> Val_none
  )



let new_def_compose def = match def with
  | [Motif_variable v, expr] ->
    (match expr with
      | Application (Variable f, e) ->
	let d1, d2 = 
	  [Motif_variable v, Application (Variable f, Variable v)],
	  [Motif_variable v, e] in
	Some (d1, d2)
      | Application (Fonction d, e) ->
	let d1, d2 = 
	  d,
	  [Motif_variable v, e] in
	Some (d1, d2)
      | _ -> None)
  | _ -> None

let primitive_compose = Val_primitive 
  (fun f -> match f with 
    | Val_fermeture {definition=def; environnement = env } -> 
      (match new_def_compose def with
	| Some (d1, d2) ->
	  let make d =  Val_fermeture{definition=d; environnement = env } in
	  Val_some (Val_paire(make d1, make d2))
	| None -> Val_none)
    | _ -> Val_none
  )


let get_const env def = match def with
  | [Motif_variable v, expr] ->
    (match expr with
      | Nombre n ->
	Some (Val_nombre n)
      | Variable v' when v <> v' -> Some (evalue env (Variable v'))
      | _ -> None)
  | _ -> None

let primitive_const = Val_primitive 
  (fun f -> match f with 
    | Val_fermeture {definition=def; environnement = env } -> 
      (match get_const env def with
	| Some c ->
	  Val_some ( c)
	| None -> Val_none)
    | _ -> Val_none
  )

let is_id def = match def with
  | [Motif_variable v, expr] ->
    (match expr with
        Variable v -> true
      | _ -> false
    )
  | _ -> false

let primitive_id = Val_primitive 
  (fun f -> match f with 
    | Val_fermeture {definition=def; environnement = env } -> 
      Val_booleenne (is_id def)
    | _ -> Val_booleenne false
  )
