open Syntax
open Modules
open Lambda_repl
open Error
open Helper

let rec matching env value pattern = match value, pattern with
  | (_, PAll) -> []
  | (value, PVariable id) ->
    if List.mem_assoc id env then
      begin
	match value, List.assoc id env with
	  | _, (_, false) -> [id, (value, false)]
	  | EVariable v, (EVariable v', true)  when v = v' -> [] (* si la variable id est de type declare, on test si value = PVariable id (when implicite) *)
	  | _ -> raise MatchingFailure
      end
    else
      [id, (value, false)] 
  | (EBoolean b1, PBoolean b2) ->
      if b1 = b2 then [] else raise MatchingFailure
  | (ENum i1, PNum i2) ->
      if i1 = i2 then [] else raise MatchingFailure
  | (EString s1, PString s2) ->
      if s1 = s2 then [] else raise MatchingFailure
  | (EPair(v1, v2), PPair (m1, m2)) ->
      matching env v1 m1 @ matching env v2 m2
  | (ENil, PNil) -> []
  | (ECons (v1, v2), PCons(m1, m2)) ->
      matching env v1 m1 @ matching env v2 m2
  | (ENone, PNone) -> []
  | (ESome v, PSome m) -> matching env v m

  | (expr, POp(op, m1, m2)) ->
      (match expr with
         | EApplication (EApplication (EVariable ope, e1), e2) when ope = op ->
             (matching env e1 m1) @ (matching env e2 m2)
         | _ -> raise MatchingFailure
      )
  | (expr, PMinus m) ->
      (match expr with
         | EApplication (EVariable "-", e) -> matching env e m
         | _ -> raise MatchingFailure
      )
  | (expr, PApplication (f, x)) ->
    (match expr with
      | EApplication(f', x') ->  (matching env f' f) @ (matching env x' x)
      | _ -> raise MatchingFailure
    )
  | (expr, PFunction(arg, pexpr)) ->
    (match expr with
      | EFunction ({def = [(PVariable v, e)]; env = Some env'}) ->
	(* on récupère une variable libre à la fois de pexpr et de expr *)
	let pfree_vars = pattern_free_vars pexpr in
	let efree_vars = free_vars expr in
	let ens = StringSet.union pfree_vars efree_vars in
	let new_arg = new_variable ens arg in
	(* on subsitutitue pexpr et expr par la nouvelle variable *)
	let expr' = substitution e (EVariable new_arg) v in
	let pexpr' = pattern_substitution pexpr new_arg arg in

	let env2 = (new_arg, (EVariable new_arg, true))::env' in
	matching env2 expr' pexpr'
      | _ -> raise MatchingFailure
    )
  | _ -> raise MatchingFailure

let value (_, v) = v
let env (e, _) = e

(* Top level definitions that change the env globally *)
let rec eval env = function
  | ELet (def, None) ->
      let (env', _) = eval_definition env def
      in (env', EUnit)
  | EDeclare(var, None) ->
    let env' = (var, (EVariable var, true))::env in
    (env', EUnit)
  | EOpen (m, None) -> 
      let env' = open_module m
      in (env', EUnit)
  | expr -> (env, eval' env expr)

(* Other expressions that only change the env locally *)
and eval' env expr = match expr with
  | EVariable s -> 
      begin try fst (List.assoc s env) with _ -> raise (Error ("Unknown " ^ s)) end

  | EFunction {def = [PVariable v, expr]; env = None}->
      let f = EFunction {def = [PVariable v, expr]; env = Some env} in
        normal_order_reduct (replace env f)

  | EFunction {def = def; env = None} -> 
      EFunction {def = def; env = Some env}

  | EApplication(f, e) ->
    let f', x' = (eval' env f, eval' env e) in
        begin match EApplication(f', x') with
	  | EApplication(EApplication(EVariable "+", ENum x), ENum y) -> ENum (x+y)
	  | EApplication(EApplication(EVariable "*", ENum x), ENum y) -> ENum (x*y)
	  | EApplication(EApplication(EVariable "/", ENum x), ENum y) ->
	    if x mod y = 0 then ENum (x/y)
	    else EApplication(f', x')
	  | EApplication(EVariable "-", ENum x) -> ENum (-x)



          | EApplication(EFunction {def = def; env = Some env_f}, arg) -> 
              eval_application env_f def arg

          | _ -> EApplication(f', x')
        end


  | EPair(e1, e2) -> EPair(eval' env e1, eval' env e2)
  | ECons(e1, e2) -> ECons(eval' env e1, eval' env e2)
  | ESome e -> ESome (eval' env e)
  | EOpen (m, Some expr) ->
      let env' = open_module m
      in eval' (env' @ env) expr
  | ELet(def, Some corps) ->
      eval' (fst (eval_definition env def)) corps
  | EDeclare(var, Some corps) ->
    let env' = (var, (EVariable var, true))::env in
    eval' env' corps
  | r -> r

and eval_application env case_list argument = match case_list with
  | [] -> raise (Error "Pattern matching failure")
  | (pattern, expr) :: rest ->
      try
        let extended_env = matching env argument pattern @ env in
          eval' extended_env expr
      with
          MatchingFailure -> eval_application env rest argument

and eval_definition curr_env def =
  match def.recursive with
    | false ->
        let valeur = eval' curr_env def.expr
        in ((def.name, (valeur, false)) :: curr_env, valeur)
    | true ->
        match def.expr with
          | EFunction f ->
              let closure = {def = f.def; env = None } in
              let new_entry = def.name, (EFunction closure, false) in
              let extended_env = new_entry :: curr_env in
                closure.env <- Some extended_env;
                (extended_env, EFunction closure)
          | _ -> raise (Error "Non-functionnal recursive let definition")

and do_eval env = function
  | [] -> env
  | x :: xs -> 
      let (env', _) = eval env x
      in do_eval (env' @ env) xs

and open_module m = 
  let handle = open_in (file_from_module m) in
  let ast = parse Parser.file (Lexing.from_channel handle) 
  in close_in handle; do_eval [] ast

