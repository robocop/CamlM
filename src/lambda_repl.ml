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

let replace env f = 
  let rec replace' fv env = function
    | EVariable x when StringSet.mem x fv  ->
      begin 
        try 
          let v, r = List.assoc x env in
            if (not r) && is_simple_value v then v
            else EVariable x
        with _ -> raise (Error ("Unknown " ^ x)) 
      end
    | EVariable x -> EVariable x
    | EApplication(m, n) ->
      EApplication(replace' fv env m, replace' fv env n)
    | EFunction {def = [PVariable v, expr]; env = Some e} ->
      EFunction {def = [PVariable v, replace' fv e expr]; env = Some e} 
    | rest -> rest
  in
  replace' (free_vars f) env f


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




let rec pattern_free_vars = function
  | PVariable x -> StringSet.singleton x
  | PFunction(arg, pexpr)->
    StringSet.diff (pattern_free_vars pexpr) (StringSet.singleton arg)     
  | PPair (p1, p2) | PCons(p1, p2) | POp(_, p1, p2) | PApplication(p1, p2) ->
    StringSet.union (pattern_free_vars p1) (pattern_free_vars p2)
  | PSome p | PMinus p -> pattern_free_vars p
  | _ -> StringSet.empty

let rec pattern_substitution expr v_arg x = match expr with
  | PVariable v when v = x -> PVariable v_arg
  | PVariable y when y <> x -> PVariable y
  | PFunction(v, pexpr) when v = x-> PFunction(v, pexpr)
  | PFunction(y, pexpr)  when y <> x ->
    let ens = StringSet.union 
      (StringSet.union (StringSet.singleton v_arg) (pattern_free_vars pexpr))
      (StringSet.singleton x) 
    in
    let z = new_variable ens y in
    let pe1 = pattern_substitution pexpr z y in
    let pe2 = pattern_substitution pe1 v_arg x in
    PFunction(z, pe2)
  | PApplication(p1, p2) ->
    PApplication(pattern_substitution p1 v_arg x, pattern_substitution p2 v_arg x)
  | PPair (p1,p2) -> 
    PPair(pattern_substitution p1 v_arg x, pattern_substitution p2 v_arg x)
  | PCons(p1, p2) -> 
    PCons(pattern_substitution p1 v_arg x, pattern_substitution p2 v_arg x)
  | POp(op, p1, p2) -> 
    POp(op, pattern_substitution p1 v_arg x, pattern_substitution p2 v_arg x)
  | PSome p -> PSome (pattern_substitution p v_arg x)
  | PMinus p ->  PMinus (pattern_substitution p v_arg x)
  | rest -> rest
    
