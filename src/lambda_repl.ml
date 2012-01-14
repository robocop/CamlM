open Syntax
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

let rec get_pattern_var = function
  | PWhen (e, m) -> get_pattern_var m
  | PVariable v -> StringSet.singleton v
  | PPair (m1, m2) -> StringSet.union (get_pattern_var m1) (get_pattern_var m2)
  | PCons (m1, m2) ->  StringSet.union (get_pattern_var m1) (get_pattern_var m2)
  | PSome m-> get_pattern_var m
  | FunP_op (_, m1, m2) ->
      StringSet.union (get_pattern_var m1) (get_pattern_var m2)
  | FunP_const m -> get_pattern_var m
  | _ -> StringSet.empty


let rec free_vars = function
  | EVariable x -> StringSet.singleton x
  | EFunction {def = def} -> StringSet.empty
  | EApplication(m, n) ->
      StringSet.union (free_vars m) (free_vars n)
  | _ -> StringSet.empty


let rec is_simple_value = function
  | EFunction {def = [PVariable _, expr]} -> is_simple_value expr
  | ENum _ | EBoolean _ | ENil | ENone | EString _ | EVariable _ -> true
  | EApplication(_, _) -> true
  | EPair (a, b) | ECons (a, b) -> 
      is_simple_value a && is_simple_value b
  | ESome e -> is_simple_value e
  | _ -> false


let rec replace' fv lv env = function
  | EVariable x when StringSet.mem x fv  ->
      begin 
        try 
          let v = List.assoc x env in
            if is_simple_value v then (v (*replace' fv lv env v *))
            else EVariable x
        with _ -> raise (Error ("Unknown " ^ x)) 
      end
  | EVariable x -> EVariable x
  | EApplication(m, n) ->
      EApplication(replace' fv lv env m, replace' fv lv env n)
  | EFunction {def = def; env = e} ->
      let replace_def d = List.map 
                            (fun (m, e) -> 
                               let variables = get_pattern_var m in
                               let lv' = StringSet.union lv variables in
                               let fv' = StringSet.diff (free_vars e) lv' in
                                 (m, replace' fv' lv' env e)
                            ) d
      in
        EFunction {def = replace_def def; env = e} 
  | rest -> rest

let replace env f = replace' (free_vars f) (StringSet.empty) env f

let rec substitution expr arg x = match expr with
  | EVariable v when v = x -> arg
  | EVariable y when y <> x -> EVariable y
  | EFunction {def = [PVariable v, e]; env = env} when v = x->
    EFunction {def = [PVariable v, e]; env = env}
  | EFunction {def = [PVariable y, e]; env = env} when y <> x ->
    let ens = StringSet.union (StringSet.union (free_vars arg) (free_vars e)) (StringSet.singleton x) in
    let z = new_variable ens y in
    let e1 = substitution e (EVariable z) y in
    let e2 = substitution e1 arg x in
    EFunction {def = [PVariable z, e2]; env = env}
  | EApplication(n1, n2) ->
    EApplication(substitution n1 arg x, substitution n2 arg x)
  | rest -> rest


let rec normal_order_reduct = function
  | EApplication
      (EFunction {def = [PVariable x, m]}, n) ->
    normal_order_reduct (substitution m n x)
  | EFunction {def = [PVariable x, m]; env = env} ->
    EFunction {def = [PVariable x, normal_order_reduct m]; env = env}
  | EApplication(m, n) ->
    EApplication(normal_order_reduct m, normal_order_reduct n)
  | rest -> rest

    
let decompose_op op = function
  | EFunction {def = [PVariable v, expr]; env = env} ->
    (match expr with
      | EApplication (EApplication (EVariable ope, e1), e2) when ope = op ->
	let make e = EFunction {def = [PVariable v, e]; env = env } in
	Some (make e1, make e2)
      | _ -> None
    )
  | _ -> None

let minus = function
  | EFunction {def = [PVariable v, expr]; env = env} ->
    (match expr with
      | EApplication (EVariable "-", e1) ->
	let f = EFunction {def = [PVariable v, e1]; env = env } in Some f
      | _ -> None
    )
  | _ -> None

let is_id = function
  | EFunction {def = [PVariable v, expr]} ->
    (match expr with
        EVariable v -> true
      | _ -> false
    )
  | _ -> false

let const = function
  | EFunction {def = [PVariable v, expr]; env = env} ->
    (match expr with
      | ENum n -> Some (ENum n)
      | _ -> None
    )
  | _ -> None

