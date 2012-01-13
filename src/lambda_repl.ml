open Syntaxe
open Error

module StringSet = Set.Make(String)

let next str = 
  let nstr = String.copy str in 
    match str.[String.length str - 1] with
      | 'z' -> nstr.[String.length str - 1] <- 'a'; nstr ^ "a"
      | c -> nstr.[String.length str - 1] <- Char.chr (Char.code c + 1);
             nstr

let rec new_variable set v = 
  if not (StringSet.mem v set) then v
  else new_variable set (next v)

let rec get_var_motif = function
  | Motif_when (e, m) -> get_var_motif m
  | Motif_variable v -> StringSet.singleton v
  | Motif_paire (m1, m2) -> StringSet.union (get_var_motif m1) (get_var_motif m2)
  | Motif_cons (m1, m2) ->  StringSet.union (get_var_motif m1) (get_var_motif m2)
  | Motif_some m-> get_var_motif m
  | FMotif_op (_, m1, m2) ->
      StringSet.union (get_var_motif m1) (get_var_motif m2)
  | FMotif_const m -> get_var_motif m
  | _ -> StringSet.empty


let rec free_bounds = function
  | Variable x -> StringSet.singleton x
  | Fonction {def = def} -> StringSet.empty
  | Application(m, n) ->
      StringSet.union (free_bounds m) (free_bounds n)
  | _ -> StringSet.empty


let rec is_simple_value = function
  | Fonction {def = [Motif_variable _, expr]} -> is_simple_value expr
  | Nombre _ | Booleen _ | Nil | CNone | String _ | Variable _ -> true
  | Application(_, _) -> true
  | Paire(a, b) | Cons(a, b) -> 
      is_simple_value a && is_simple_value b
  | CSome e -> is_simple_value e
  | _ -> false


let rec remplacement fv lv env = function
  | Variable x when StringSet.mem x fv  ->
      begin 
        try 
          let v = List.assoc x env in
            if is_simple_value v then (v (*remplacement fv lv env v *))
            else Variable x
        with _ -> raise (Erreur (x ^ " non connu")) 
      end
  | Variable x -> Variable x
  | Application(m, n) ->
      Application(remplacement fv lv env m, remplacement fv lv env n)
  | Fonction {def = def; environnement = e} ->
      let replace_def d = List.map 
                            (fun (m, e) -> 
                               let variables = get_var_motif m in
                               let lv' = StringSet.union lv variables in
                               let fv' = StringSet.diff (free_bounds e) lv' in
                                 (m, remplacement fv' lv' env e)
                            ) d
      in
        Fonction {def = replace_def def; environnement = e} 
  | rest -> rest

let replace env f = remplacement (free_bounds f) (StringSet.empty) env f

let rec substitution expr arg x = match expr with
  | Variable v when v = x -> arg
  | Variable y when y <> x -> Variable y
  | Fonction {def = [Motif_variable v, e]; environnement = env} when v = x->
    Fonction {def = [Motif_variable v, e]; environnement = env}
  | Fonction {def = [Motif_variable y, e]; environnement = env} when y <> x ->
    let ens = StringSet.union (StringSet.union (free_bounds arg) (free_bounds e)) (StringSet.singleton x) in
    let z = new_variable ens y in
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

let minus = function
  | Fonction {def = [Motif_variable v, expr]; environnement = env} ->
    (match expr with
      | Application (Variable "-", e1) ->
	let f = Fonction {def = [Motif_variable v, e1]; environnement = env } in Some f
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

