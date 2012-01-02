open Syntaxe
exception Echec_filtrage
exception Erreur of string;;
let scope = ref []

module StringSet = Set.Make(String) ;;  

let next str = 
  let nstr = String.copy str in 
  match str.[String.length str - 1] with
    | 'z' -> nstr.[String.length str - 1] <- 'a'; nstr ^ "a"
    | c -> nstr.[String.length str - 1] <- Char.chr (Char.code c + 1);
      nstr
;;
let rec new_variable l v = 
  let v' = next v in
  if not (List.mem v' l) then v'
  else new_variable l v'
;;
let rec free_bounds = function
  | Variable x -> StringSet.singleton x
  | Fonction {def = [Motif_variable x, m]} ->
    StringSet.diff (free_bounds m) (StringSet.singleton x)
  | Application(m, n) ->
    StringSet.union (free_bounds m) (free_bounds n)
  | _ -> StringSet.empty

let rec remplacement fv env = function
  | Variable x when StringSet.mem x fv && not (List.mem x ["+"; "*"; "-"; "/"])  ->
     begin try List.assoc x env with _ -> raise (Erreur (x ^ " non connu")) end
  | Variable x -> Variable x
  | Application(m, n) ->
    Application(remplacement fv env m, remplacement fv env n)
  | rest -> rest

let rec remplace f = match f with
  | Fonction {def = [Motif_variable v, e]; environnement = Some env} ->
    let e' = remplacement (free_bounds f) env e in
    Fonction {def = [Motif_variable v, e']; environnement = Some env} 
  | _ -> f
;;

let rec substitution expr arg x = match expr with
  | Variable v when v = x -> arg
  | Variable y when y <> x -> Variable y
  | Fonction {def = [Motif_variable v, e]; environnement = env} when v = x->
    Fonction {def = [Motif_variable v, e]; environnement = env}
  | Fonction {def = [Motif_variable y, e]; environnement = env} when y <> x ->
    let z = "z" in
    let e1 = substitution e (Variable z) y in
    let e2 = substitution e1 arg x in
    Fonction {def = [Motif_variable z, e2]; environnement = env}
  | Application(n1, n2) ->
    Application(substitution n1 arg x, substitution n2 arg x)
  | rest -> rest


let rec normal_order_reduct = function
  | Application
      (Fonction {def = [Motif_variable x, m]}, n) ->
    normal_order_reduct (substitution m n x)
  | Fonction {def = [Motif_variable x, m]; environnement = env} ->
    Fonction {def = [Motif_variable x, normal_order_reduct m]; environnement = env}
  | Application(m, n) ->
    Application(normal_order_reduct m, normal_order_reduct n)
  | rest -> rest

let decompose_op op = function
  | Fonction {def = [Motif_variable v, expr]; environnement = env} ->
    (match expr with
      | Application (Application (Variable ope, e1), e2) when ope = op ->
	let make e = Fonction {def = [Motif_variable v, e]; environnement = env } in
	Some (make e1, make e2)
      | _ -> None
    )
  | _ -> None

let is_id = function
  | Fonction {def = [Motif_variable v, expr]} ->
    (match expr with
        Variable v -> true
      | _ -> false
    )
  | _ -> false

let const = function
  | Fonction {def = [Motif_variable v, expr]; environnement = env} ->
    (match expr with
      | Nombre n -> Some (Nombre n)
      | _ -> None
    )
  | _ -> None

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
  | (f, FMotif_const m) ->
    (match const f with
      | Some v -> filtrage v m
      | None -> raise Echec_filtrage
    )
  | (f, FMotif_op(op, m1, m2)) ->
    (match decompose_op op f with
      | Some (f1, f2) ->
	(filtrage f1 m1) @ (filtrage f2 m2)
      | None -> raise Echec_filtrage
      )
  | (f, FMotif_Id) ->
     if is_id f then []
     else raise Echec_filtrage

  | _ -> raise Echec_filtrage

let rec evalue env expr = match expr with
  | Variable s ->
    begin try List.assoc s env with _ -> raise (Erreur (s ^ " non connu")) end

  | Fonction {def = [Motif_variable v, expr]; environnement = None}->
    let f = Fonction {def = [Motif_variable v, expr]; environnement = Some env} in
    normal_order_reduct (remplace f)

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
    evalue (fst (evalue_definition env def)) corps
  | Let(def, None) ->
    let (scope', valeur) = evalue_definition !scope def
    in scope := scope'; valeur
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
      in ((def.nom, valeur)::env_courant, valeur)
    | true ->
      match def.expr with
	| Fonction ferm ->
	  let fermeture = {def = ferm.def; environnement = None } in
	  let env_etendu = (def.nom, Fonction fermeture)::env_courant in
	  fermeture.environnement <- Some env_etendu;
	  (env_etendu, Fonction fermeture)
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
  | Application (Application (Variable op, e1), e2) when List.mem op ["+"; "*"; "-"; "/"] -> Printf.sprintf "(%s %s %s)" (imprime e1) op (imprime e2)
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
