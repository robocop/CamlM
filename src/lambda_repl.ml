open Syntax
open Error
open Helper

let rec free_vars = function
  | EVariable x -> StringSet.singleton x
  | EFunction {def = [PVariable v, e]; env = env} ->
    StringSet.diff (free_vars e) (StringSet.singleton v)
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

let rec replace' fv env = function
  | EVariable x when StringSet.mem x fv  ->
      begin 
        try 
          let v = List.assoc x env in
            if is_simple_value v then v
            else EVariable x
        with _ -> raise (Error ("Unknown " ^ x)) 
      end
  | EVariable x -> EVariable x
  | EApplication(m, n) ->
      EApplication(replace' fv env m, replace' fv env n)
  | EFunction {def = [PVariable v, expr]; env = Some e} ->
        EFunction {def = [PVariable v, replace' fv e expr]; env = Some e} 
  | rest -> rest

let replace env f = replace' (free_vars f) env f

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
