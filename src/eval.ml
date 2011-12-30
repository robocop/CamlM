open Syntaxe
exception Echec_filtrage
exception Erreur of string;;




let rec filtrage valeur motif = match valeur, motif with
  | (_, Motif_all) -> []
  | (valeur, Motif_variable id) -> [id, valeur]
  | (Booleen b1, Motif_booleen b2) ->
    if b1 = b2 then [] else raise Echec_filtrage
  | (Nombre i1, Motif_nombre i2) ->
    if i1 = i2 then [] else raise Echec_filtrage
  | (String s1, Motif_string s2) ->
    if s1 = s2 then [] else raise Echec_filtrage
  | (Paire(v1, v2), Motif_paire (m1, m2)) ->
    filtrage v1 m1 @ filtrage v2 m2
  | (Nil, Motif_nil) -> []
  | (Cons (v1, v2), Motif_cons(m1, m2)) ->
    filtrage v1 m1 @ filtrage v2 m2
  | (CNone, Motif_none) -> []
  | (CSome v, Motif_some m) -> filtrage v m
  | _ -> raise Echec_filtrage


let rec evalue env expr = match expr with
  | Variable s ->
    begin try List.assoc s env with _ -> raise (Erreur (s ^ " non connu")) end
  | Fonction {def = def; environnement = None} -> 
     Fonction {def = def; environnement = Some env}
  | Application(f, e) ->
    let eval_f = evalue env f in
    let eval_e = evalue env e in
    begin match eval_f with
      | Primitive(n, f) -> f eval_e
      | Fonction {def = def; environnement = Some env_f} -> 
	evalue_application env_f def eval_e
      | _ -> raise (Erreur "application d'une valeur non fonctionelle")
    end
  | Paire(e1, e2) -> Paire(evalue env e1, evalue env e2)
  | Cons(e1, e2) -> Cons(evalue env e1, evalue env e2)
  | CSome e -> CSome (evalue env e)
  | Let(def, Some corps) ->
    evalue (evalue_definition env def) corps
  | r -> r
and evalue_application env list_de_cas argument = match list_de_cas with
  | [] -> raise (Erreur "echec du filtrage")
  | (motif, expr) :: autres_cas ->
    try
      let env_etendu = filtrage argument motif @ env in
      evalue env_etendu expr
    with
	Echec_filtrage -> evalue_application env autres_cas argument


and evalue_definition env_courant def =
  match def.recursive with
    | false ->
      let valeur = evalue env_courant def.expr
      in (def.nom, valeur)::env_courant
    | true ->
      match def.expr with
	| Fonction ferm ->
	  let fermeture = {def = ferm.def; environnement = None } in
	  let env_etendu = (def.nom, Fonction fermeture)::env_courant in
	  fermeture.environnement <- Some env_etendu;
	  (* (env, Fonction fermeture) *) env_etendu
	| _ -> raise (Erreur "let rec non fonctionnel") 




let rec print_def def = 
  Printf.sprintf "let %s%s = %s" 
    (if def.recursive then "rec " else "") 
    def.nom 
    (imprime def.expr)
and imprime = function
  | Variable v -> v
  | Nombre n -> string_of_int n
  | String s -> Printf.sprintf "\"%s\"" s
  | Booleen false -> "false"
  | Booleen true -> "true"
  | Paire(e1, e2) ->
    "("^imprime e1^", "^imprime e2^")"
  | Nil -> "[]"
  | Cons(e1, e2) ->
    imprime e1 ^ "::" ^imprime e2
  | Application(f, e) ->
    "("^imprime f^") "^"("^imprime e^")" 
  | Fonction {def = def; environnement = _} -> 
    begin
      match def with
	| [Motif_variable v, expr] ->
	   Printf.sprintf "\\%s -> %s" v  (imprime expr)
	| _ -> "<fun>"
    end
  | Primitive (s, _) -> s
  | CNone -> "None"
  | CSome e -> Printf.sprintf "Some %s" (imprime e) 
  | Let(def, Some expr) ->
    (print_def def) ^ " in " ^ imprime expr
  | Let(def, None) -> print_def def
;;
