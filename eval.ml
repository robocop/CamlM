open Syntaxe

type valeur = 
  | Val_nombre of int
  | Val_booleenne of bool
  | Val_paire of valeur * valeur
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

let scope = ref [];;

let fst = function (x, _) -> x;;
let snd = function (_, y) -> y;;

let rec imprime_valeur = function
  | Val_nombre n -> string_of_int n
  | Val_booleenne false -> "false"
  | Val_booleenne true -> "true"
  | Val_paire(v1, v2) ->
    "("^imprime_valeur v1^", "^imprime_valeur v2^")"
  | Val_nil -> "[]"
  | Val_cons(v1, v2) ->
    imprime_valeur v1 ^ "::" ^imprime_valeur v2
  | Val_fermeture _ | Val_primitive _ -> "<fun>"
  | Val_none -> "None"
  | Val_some v -> Printf.sprintf "Some (%s)" (imprime_valeur v) 
;;


let rec filtrage valeur motif = match valeur, motif with
  | (valeur, Motif_variable id) -> [id, valeur]
  | (Val_booleenne b1, Motif_booleen b2) -> 
    if b1 = b2 then [] else raise Echec_filtrage
  | (Val_nombre i1, Motif_nombre i2) ->
    if i1 = i2 then [] else raise Echec_filtrage
  | (Val_paire(v1, v2), Motif_paire (m1, m2)) -> 
    filtrage v1 m1 @ filtrage v2 m2
  | (Val_nil, Motif_nil) -> []
  | (Val_cons (v1, v2), Motif_cons(m1, m2)) ->
    filtrage v1 m1 @ filtrage v2 m2
  | (Val_none, Motif_none) -> []
  | (Val_some v, Motif_some m) -> filtrage v m
  | _, _ -> raise Echec_filtrage
;;

let rec evalue env expr = match expr with
  | Variable s -> 
    begin try List.assoc s env with _ -> raise (Erreur (s ^ " non connu")) end
  | Fonction liste_de_cas ->
    Val_fermeture {definition = liste_de_cas; environnement = env} 
  | Application(f, a) ->
    let val_fonction = evalue env f in
    let val_argument = evalue env a in
    begin match val_fonction with
      | Val_primitive fonction_primitive -> fonction_primitive val_argument
      | Val_fermeture fermeture -> evalue_application fermeture.environnement fermeture.definition val_argument
      | _ -> raise (Erreur "Application d'une valeur non fonctionelle")
    end
  | Let (def, None) ->
    let (scope', valeur) = evalue_definition !scope def
    in scope := scope'; valeur
  | Let (def, Some corps) ->
    evalue (fst (evalue_definition env def)) corps
  | Booleen b -> Val_booleenne b
  | Nombre n -> Val_nombre n
  | Paire(e1, e2) -> Val_paire(evalue env e1, evalue env e2)
  | Nil -> Val_nil
  | Cons(e1, e2) -> Val_cons(evalue env e1, evalue env e2)

  | CNone -> Val_none
  | CSome e -> Val_some (evalue env e)

and evalue_application env liste_de_cas arg = match liste_de_cas with
  | [] -> raise (Erreur "echec du filtrage")
  | (motif, expr):: autres_cas ->
    try 
      let env_etendu =  (filtrage arg motif) @ env in
      evalue env_etendu expr
    with Echec_filtrage -> evalue_application env autres_cas arg

and evalue_definition env_courant def = 
  match def.recursive with
    | false -> 
      let valeur = evalue env_courant def.expr
      in ((def.nom, valeur)::env_courant, valeur)
    | true ->
      match def.expr with
	| Fonction liste_de_cas ->
	  let fermeture = {definition = liste_de_cas; environnement = [] } in
	  let env = (def.nom, Val_fermeture fermeture)::env_courant in
	  fermeture.environnement <- env; 
	  (env, Val_fermeture fermeture)
	| _ -> raise (Erreur "let rec non fonctionnel")
;;
