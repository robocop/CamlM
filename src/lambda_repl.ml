open Syntax
open Error
open Helper

(* Définit récursivement les expressions qui seront réduites par des opérations de lambda calcul. *)
(* f est dite de type 'fonction formelle' si is_simple_value f                                    *)
(* Seules les fonctions formelles sont réduites par replace et normal_order_reduct                *)
let rec is_simple_value = function
  | EFunction {def = [PVariable _, expr]; env = _} -> is_simple_value expr
  | ENum _ | EBoolean _ | ENil | ENone | EString _ | EVariable _ -> true
  | EPair (a, b) | ECons (a, b) | EApplication(a, b)-> 
      is_simple_value a && is_simple_value b
  | ESome e -> is_simple_value e
  | f -> false


(* Calcule les variables libres (ie celle qui sont présent dans l'environnemet global) d'une fonction formelle *)
let rec free_vars = function
  | EVariable x -> StringSet.singleton x
  | EFunction {def = [PVariable v, e]; env = _} ->
    StringSet.diff (free_vars e) (StringSet.singleton v)
  | EApplication(m, n) 
  | EPair(m, n)
  | ECons(m, n) ->
      StringSet.union (free_vars m) (free_vars n)
  | ESome n -> free_vars n
  | _ -> StringSet.empty


(* Réalise la substitution d'une variable (x) par une autre expression (arg) dans expr *)
(* La substitution est définie récursivement par les règles de lambda calcul           *)
let rec substitution expr arg x = match expr with
  | EVariable v when v = x -> arg
  | EVariable y when y <> x -> EVariable y
  | EFunction {def = [PVariable v, e]; env = env} when v = x->
    EFunction {def = [PVariable v, e]; env = env}
  | EFunction {def = [PVariable y, e]; env = env} as f when y <> x ->
    let ens = StringSet.union (StringSet.union (free_vars arg) (free_vars f)) (StringSet.singleton x) in
    let z = new_variable ens y in
    let e1 = substitution e (EVariable z) y in
    let e2 = substitution e1 arg x in
    EFunction {def = [PVariable z, e2]; env = env}
  | EApplication(n1, n2) -> 
    EApplication(substitution n1 arg x, substitution n2 arg x)
  | EPair(n1, n2) -> EPair(substitution n1 arg x, substitution n2 arg x)
  | ECons(n1, n2) -> ECons(substitution n1 arg x, substitution n2 arg x)
  | ESome(n) -> ESome(substitution n arg x)
  | rest -> rest


(* Remplace toutes les variables libres de type is_simple_value d'une expression par leur expression dans l'environnement env *)
(* Utilise substitution pour faire un remplacement valide                                                                     *)
let replace env f = 
  let rec to_replace fv env = function
    | EVariable x when StringSet.mem x fv ->
      begin 
        try 
          (match List.assoc x env with
	    | Some v  -> if is_simple_value v then StringSet.singleton x else StringSet.empty
	    | None -> StringSet.empty
	  )
        with _ -> raise (Error ("Unknown " ^ x)) 
      end
    | EVariable x -> StringSet.empty
    | EPair(m, n)
    | ECons(m, n) 
    | EApplication(m, n) ->
      StringSet.union (to_replace fv env m) (to_replace fv env n)
    | EFunction {def = [PVariable v, expr]; env = _} ->
      to_replace fv env expr
    | ESome n -> to_replace fv env n
    | rest -> StringSet.empty
  in
  StringSet.fold 
    (fun var expr -> substitution expr (get (List.assoc var env)) var)
    (to_replace (free_vars f) env f)
    f


(* Effectue une beta reduction *)
let rec normal_order_reduct = function
  | EApplication
      (EFunction {def = [PVariable x, m]}, n) ->
    normal_order_reduct (substitution m n x)
  | EFunction {def = [PVariable x, m]; env = env} ->
    EFunction {def = [PVariable x, normal_order_reduct m]; env = env}
  | EApplication(m, n)  -> EApplication(normal_order_reduct m, normal_order_reduct n)
  | EPair(m, n) -> EPair(normal_order_reduct m, normal_order_reduct n)
  | ECons(m, n) ->
    ECons(normal_order_reduct m, normal_order_reduct n)
  | ESome n -> normal_order_reduct n
  | rest -> rest

