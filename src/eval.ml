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

  | (expr, POp(op, pf, pg)) ->
    (match expr with
         | EFunction
	     ({def = [PVariable v, 
		      EApplication(EApplication (EVariable op', e1), e2)]; 
	      env = envi}) when op = op' ->
           let f = EFunction({def = [PVariable v, e1]; env = envi }) in
	   let g = EFunction({def = [PVariable v, e2]; env = envi }) in
           (matching env f pf) @ (matching env g pg)
         | _ -> raise MatchingFailure
    )

  | (expr, PMinus m) ->
      (match expr with
         | EFunction
	     ({def = [PVariable v, EApplication (EVariable "-", e)]; 
	      env = envi}) ->
            let f' = EFunction({def = [PVariable v, e]; env = envi }) in
            matching env f' m
         | _ -> raise MatchingFailure
      )
  | (expr, PCompose (pf, pg)) ->
    (match expr with
         | EFunction
	     ({def = [PVariable v, EApplication(e1, e2)]; env = envi})  ->
           let f = match e1 with
	     | EVariable f -> EVariable f
	     | (EFunction _) as f -> f
	     | _ -> EFunction({def = [PVariable v, EApplication(e1, EVariable v)]; env = envi })
	   in
	   let g = EFunction({def = [PVariable v, e2]; env = envi }) in
           (matching env f pf) @ (matching env g pg)
         | _ -> raise MatchingFailure
    )
  | (expr, PIdentity) ->
    (match expr with
       EFunction({def = [PVariable v, EVariable v']}) when v = v' -> []
      | _ -> raise MatchingFailure
    )
  | (expr, PConst p) ->
     (match expr with
       EFunction({def = [PVariable v, e]})  -> 
	 let fv = free_vars e in
	 if StringSet.mem v fv then raise MatchingFailure
	 else matching env e p
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
      let env' = open_module env m
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
	  | EApplication(EApplication(EVariable "==", a), b) -> EBoolean (a = b)
	  | EApplication(EApplication(EVariable "&&", EBoolean x), EBoolean y) -> EBoolean (x&&y)
	  | EApplication(EApplication(EVariable "||", EBoolean x), EBoolean y) -> EBoolean (x||y)


          | EApplication(EFunction {def = def; env = Some env_f}, arg) -> 
              eval_application env_f def arg

          | _ -> EApplication(f', x')
        end


  | EPair(e1, e2) -> EPair(eval' env e1, eval' env e2)
  | ECons(e1, e2) -> ECons(eval' env e1, eval' env e2)
  | ESome e -> ESome (eval' env e)
  | EOpen (m, Some expr) ->
      let env' = open_module env m
      in eval' env' expr
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
      in do_eval env' xs

and open_module env m = 
  let handle = open_in (file_from_module m) in
  let ast = parse Parser.file (Lexing.from_channel handle) 
  in close_in handle; do_eval env ast

