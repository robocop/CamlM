open Syntax
open Modules
open Lambda_repl
open Error
open Helper

let op_property prop op env = match op_prop (lookup_fun_env op env) with
  | None -> raise (Error "tried to get op properties from a non-op object")
  | Some p -> List.mem prop p

let is_com = op_property Com
let is_assoc = op_property Assoc

(* Fait correspondre une expression à un pattern, et calcule les nouvelles variables   *)
(* Renvoit un bout d'environnement que l'on colle à l'environnement précédant         *)
let rec matching (com_test, assoc_test) env value pattern = match value, pattern with
  | (_, PAll) -> []
  | (value, PAxiom id) ->
      begin
        match value, lookup_fun_env id env with
          | EVariable v, (_, (None, _)) when v = id -> []
          | _ -> raise MatchingFailure
      end

  | (value, PVariable id) -> [(id, Some value, None)] 
  | (EBoolean b1, PBoolean b2) ->
      if b1 = b2 then [] else raise MatchingFailure
  | (ENum i1, PNum i2) ->
      if i1 = i2 then [] else raise MatchingFailure
  | (EString s1, PString s2) ->
      if s1 = s2 then [] else raise MatchingFailure
  | (EPair(v1, v2), PPair (m1, m2)) ->
      matching (false, false) env v1 m1 @ matching (false, false) env v2 m2
  | (ENil, PNil) -> []
  | (ECons (v1, v2), PCons(m1, m2)) ->
      matching (false, false) env v1 m1 @ matching (false, false) env v2 m2
  | (ENone, PNone) -> []
  | (ESome v, PSome m) -> matching (false, false) env v m
  | (expr, POp(op1, POp(op2, a, b), c)) when op1 = op2 && is_assoc op1 env && not assoc_test->
      begin
        try matching (com_test, true) env expr pattern 
        with MatchingFailure -> matching (com_test, true) env expr  (POp(op1, a, POp(op2, b, c)))
      end
  | (expr, POp(op, a, b)) when is_com op env && not com_test ->
      (try matching (true, assoc_test) env expr pattern 
       with MatchingFailure -> matching (true, assoc_test) env expr (POp(op, b, a))
      )
  | (expr, POp(op, pf, pg)) ->
      begin
        match expr with
          | EFunction (
              {def = [PVariable v, EApplication(EApplication (EVariable op', e1), e2)];
               env = envi}) when op = op' ->
              let f = EFunction({def = [PVariable v, e1]; env = envi }) in
              let g = EFunction({def = [PVariable v, e2]; env = envi }) in
                (matching (false, false) env f pf) @ (matching (false, false) env g pg)
          | _ -> raise MatchingFailure
      end 

  | (expr, PMinus m) ->
      begin
        match expr with
          | EFunction
              ({def = [PVariable v, EApplication (EVariable "-", e)]; 
                env = envi}) ->
              let f' = EFunction({def = [PVariable v, e]; env = envi }) in
                matching (false, false) env f' m
          | _ -> raise MatchingFailure
      end
  | (expr, PCompose (pf, pg)) ->
      begin
        match expr with
          | EFunction
              ({def = [PVariable v, EApplication(e1, e2)]; env = envi})  ->
              let f = match e1 with
                | EVariable f -> EVariable f
                | (EFunction _) as f -> f
                | _ -> raise MatchingFailure
              in
              let g = EFunction({def = [PVariable v, e2]; env = envi }) in
                (matching (false, false) env f pf) @ (matching (false, false) env g pg)
          | _ -> raise MatchingFailure
      end
  | (expr, PIdentity) ->
      begin
        match expr with
            EFunction({def = [PVariable v, EVariable v']}) when v = v' -> []
          | _ -> raise MatchingFailure
      end
  | (expr, PConst p) ->
      begin
        match expr with
            EFunction({def = [PVariable v, e]})  -> 
              let fv = free_vars e in
                if StringSet.mem v fv then raise MatchingFailure
                else matching (false, false) env e p
          | _ -> raise MatchingFailure
      end
  | (expr, PIsnum p) ->
      begin
        match expr with
          | ENum x -> matching (false, false) env expr p
          | _ -> raise MatchingFailure
      end
  | (expr, PWhen(cond, p)) ->
      let r = matching (false, false) env expr p in
      let env' = multi_add_env r env  in
        begin
          match eval env' cond with
            | _, (EBoolean true) -> r
            | _ -> raise MatchingFailure
        end
  | _ -> raise MatchingFailure


(* Top level definitions that change the env globally *)
and eval env = function
  | ELet (def, None) ->
      let (env', _) = eval_definition env def
      in (env', EUnit)
  | EDeclare(var, None) ->
      let env' = add_env (var, None, None) env
      in (env', EUnit)
  | EOpen (m, None) -> 
      let env' = open_fun_module m env
      in (env', EUnit)
  | expr -> (env, eval' env expr)

(* Other expressions that only change the env locally *)
(* eval' réduit récursivement une expression 'le plus possible'  *)
and eval' env expr = match expr with
  | EVariable s -> 
      begin  
        match lookup_fun_env s env with
          | (_, (Some e, _)) -> e
          | (_, (None, _)) -> EVariable s
      end
  | EFunction {def = [PVariable v, expr]; env = None}-> (* Seules les fonctions formelles sont réduites par lambda calcul *)
      let f = EFunction {def = [PVariable v, expr]; env = Some env} in
        if is_simple_value f then normal_order_reduct (replace env f)
        else f

  | EFunction {def = def; env = None} -> 
      EFunction {def = def; env = Some env}

  | EApplication(f, e) -> (* On evalue quelques primitives *)
      let f', x' = (eval' env f, eval' env e) in
        begin match f', x' with
          | EApplication(EVariable "+", ENum x), ENum y -> ENum (Int32.add x y)
          | EApplication(EVariable "*", ENum x), ENum y -> ENum (Int32.mul x y)
          | EApplication(EVariable "/", ENum x), ENum y ->
              if Int32.rem x y = Int32.zero then ENum (Int32.div x y)
              else EApplication(f', x')
          | EVariable "-", ENum x -> ENum (Int32.neg x)
          | EApplication(EVariable "^", ENum x), ENum y -> ENum (puis x y)
          | EApplication(EVariable "mod", ENum x), ENum y -> ENum (Int32.rem x y)
          | EApplication(EVariable "==", a), b -> EBoolean (a = b)
          | EApplication(EVariable "&&", EBoolean x), EBoolean y -> EBoolean (x&&y)
          | EApplication(EVariable "||", EBoolean x), EBoolean y -> EBoolean (x||y)
          | EApplication(EVariable "<=", ENum x), ENum y -> EBoolean (x<=y)
          | EApplication(EVariable ">=", ENum x), ENum y -> EBoolean (x>=y)
          | EApplication(EVariable "<", ENum x), ENum y -> EBoolean (x<y)
          | EApplication(EVariable ">", ENum x), ENum y -> EBoolean (x>y)
          | EApplication(EVariable "++", EString x), EString y -> EString (x^y)
          | EVariable "string_of_int", ENum x -> EString (Int32.to_string x)
          | EFunction {def = def; env = Some env_f}, arg -> 
              eval_application env_f def arg
          | _ -> EApplication(f', x')
        end

  | EPair(e1, e2) -> EPair(eval' env e1, eval' env e2)
  | ECons(e1, e2) -> ECons(eval' env e1, eval' env e2)
  | ESome e -> ESome (eval' env e)
  | EOpen (m, Some expr) ->
      let env' = open_fun_module m env
      in eval' env' expr 
  | ELet(def, Some corps) ->
      eval' (fst (eval_definition env def)) corps
  | EDeclare(var, Some corps) ->
      let env' = add_env (var, None, None) env
      in eval' env' corps
  | r -> r

and eval_application env case_list argument = match case_list with
  | [] -> raise (Error "Pattern matching failure")
  | (pattern, expr) :: rest ->
      try
        let extended_env = multi_add_env (matching (false, false) env argument pattern) env
        in eval' extended_env expr
      with
          MatchingFailure -> eval_application env rest argument

and eval_definition curr_env def =
  match def.recursive with
    | false ->
        let value = eval' curr_env def.expr in
        let curr_env' = add_env (def.name, Some value, None) curr_env
        in (curr_env', value)
    | true -> (* Evaluation d'une définition récusive *)
        match def.expr with
          | EFunction f ->
              let closure = {def = f.def; env = None } in
              let new_entry = (def.name, Some (EFunction closure), None) in
              let extended_env = add_env new_entry curr_env in
                closure.env <- Some extended_env;
                (extended_env, EFunction closure)
          | _ -> raise (Error "Non-functionnal recursive let definition")

and do_eval env = function
  | [] -> env
  | x :: xs -> 
      let (env', _) = eval env x
      in do_eval env' xs

and open_fun_module m env= open_module do_eval m env

