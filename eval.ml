open Syntaxe

type valeur = 
  | Val_nombre of int
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

let scope = ref [];;

let fst = function (x, _) -> x;;
let snd = function (_, y) -> y;;



let rec filtrage valeur motif = match valeur, motif with
  | (_, Motif_all) -> []
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


let applique fonction arg = match fonction with
  | Fonction [Motif_variable x, expr] -> 
    let rec remplace = function
      | Variable s when s = x -> arg
      | Application (e1, e2) ->
	Application (remplace e1, remplace e2)
      | Paire(e1, e2) -> Paire(remplace e1, remplace e2)
      | Cons(e1, e2) -> Cons(remplace e1, remplace e2)
      | CSome e -> CSome (remplace e)
      | rest -> rest
    in
    remplace expr
  | _ -> Application(fonction, arg)
let rec evalue_fonction env list_cas = match list_cas with
  | [Motif_variable x, expr] ->
    let rec replace = function
      | Application(expr1, expr2) ->
	applique (replace expr1) (replace expr2)
      | Variable s when s <> x -> 
	(match List.assoc s env with 
	  | Val_fermeture {definition= def; environnement = env'} ->
	    Fonction (evalue_fonction env' def)
	  | _ -> Variable s
	)
      | Paire(e1, e2) -> Paire(replace e1, replace e2)
      | Cons(e1, e2) -> Cons(replace e1, replace e2)
      | CSome e -> CSome (replace e)
      | rest -> rest
    in
    [Motif_variable x, replace expr]
  | _ -> list_cas

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

let rec zipn = function
  | [l1] -> l1
  | l :: ls -> zip2 l (zipn ls)
  | _ -> raise (Erreur "Cannot zip non-list values")

let flattenPair p = 
  let rec aux = function
    | Val_paire (a, b) -> a :: (aux b)
    | a -> [a]
  in Val_nuple (aux p) 

let rec valMap f = function
  | Val_nil -> Val_nil
  | Val_cons (x, xs) -> Val_cons (f x, valMap f xs)
  | _ -> raise (Erreur "Cannot map on a non list value")

let rec evalue env expr = match expr with
  | Variable s -> 
    begin try List.assoc s env with _ -> raise (Erreur (s ^ " non connu")) end
  | Fonction liste_de_cas ->
    Val_fermeture 
      {definition = evalue_fonction env liste_de_cas; 
       environnement = env} 

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

  | ListComp (e, gens) ->
    evalue_list_comp env e gens

  | CNone -> Val_none
  | CSome e -> Val_some (evalue env e)

and evalue_list_comp env e gens = 
  let ev_gens = List.map (function x -> evalue env (snd x)) gens in
  let combs = valMap flattenPair (zipn ev_gens) in
  let rec getE = function
    | Val_nil -> Val_nil
    | Val_cons (Val_nuple x, xs) -> 
          let env' = (List.combine (List.map fst gens) x) @ env
          in Val_cons (evalue env' e, getE xs)
    | _ -> raise (Erreur "Not possible")
  in getE combs

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



let rec print_fonction env = function
  | [Motif_variable v, expr] -> 
    let rec print_expr = function
      | Variable s -> s
      | Fonction f -> print_fonction env f
      | Application (Application (Variable op, e1), e2) when List.mem op ["+"; "*"; "-"; "/"] -> Printf.sprintf "(%s %s %s)" (print_expr e1) op (print_expr e2)
      | Application (Variable f, e2) -> 
	Printf.sprintf "%s (%s)" f (print_expr e2)
      | Application (e1, e2) -> Printf.sprintf "(%s) (%s)" (print_expr e1) (print_expr e2)
      | Nombre n -> string_of_int n
      | _ -> "<expr>"
    in
    Printf.sprintf "\\%s -> %s" v  (print_expr expr)
  | _ -> "<fun>"

and imprime_valeur = function
  | Val_nombre n -> string_of_int n
  | Val_booleenne false -> "false"
  | Val_booleenne true -> "true"
  | Val_paire(v1, v2) ->
    "("^imprime_valeur v1^", "^imprime_valeur v2^")"
  | Val_nuple vs ->
    "(" 
    ^ List.fold_left (fun acc x -> acc ^ ", " ^ imprime_valeur x) 
        (imprime_valeur (List.hd vs))
        (List.tl vs)
    ^ ")"
  | Val_nil -> "[]"
  | Val_cons(v1, v2) ->
    imprime_valeur v1 ^ "::" ^imprime_valeur v2
  | Val_primitive _ -> "<fun>"
  | Val_fermeture {definition=def; environnement=env} -> print_fonction env def 
  | Val_none -> "None"
  | Val_some v -> Printf.sprintf "Some %s" (imprime_valeur v) 
;;
