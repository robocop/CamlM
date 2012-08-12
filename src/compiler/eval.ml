(** Expression evaluation routines.
   
    Handles pattern matching & expression reduction and evaluation. Some
    reduction (formal function reduction) is done by {!Lambda_repl}.

    TODO : Fix name clashes when redefining Prelude names.
  *)
open Syntax
open Modules
open Lambda_repl
open Error
open Helper

let op_property prop op env = match op_prop (lookup_env op env) with
  | None -> raise (Error "tried to get op properties from a non-op object")
  | Some p -> List.mem prop p

let is_com = op_property Com
let is_assoc = op_property Assoc

(** Matches an expression against a pattern, and computes the new variables
    defined by the pattern. Returns a list of names and their values to be added
    to the environment.
   
    Operator properties (commutativity and associativity) are also handled here
    using the booleans [com_test] and [assoc_test] and the environment.
  *)
let rec matching (com_test, assoc_test) env value pattern = match value, pattern with
  | (_, PAll) -> []
  | (value, PAxiom id) ->
      begin
        match value, lookup_env id env with
          | EVariable v, (_, _) when v = id -> []
          | _ -> raise MatchingFailure
      end

  | (value, PVariable id) -> [(id, (Some value, None))] 
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
  
(*
| (_, POp("+", a, PMinus b)) when not assoc_test ->
    (try
       matching (com_test, true) env value pattern 
    with MatchingFailure -> (print_endline "ok"; matching (com_test, true) env value (POp("+", a, POp("*", PConst(PNum minus_one), b))))
    )
  | (_, POp("+", a, POp("*", PConst(PNum minus_one), b))) when not assoc_test ->
    (try
       print_endline "ok1"; 
       matching (com_test, true) env value pattern 
    with MatchingFailure -> (print_endline "ok"; matching (com_test, true) env value ( POp("+", a, PMinus b)))
    )
*)  
| (expr, POp(op1, POp(op2, a, b), c)) when op1 = op2 && is_assoc op1 env && not assoc_test->
      begin
        try matching (com_test, true) env expr pattern 
        with MatchingFailure -> matching (com_test, true) env expr  (POp(op1, a, POp(op2, b, c)))
      end
  | (expr, POp(op1, a, POp(op2, b, c))) when op1 = op2 && is_assoc op1 env && not assoc_test->
      begin
        try matching (com_test, true) env expr pattern 
        with MatchingFailure -> matching (com_test, true) env expr  (POp(op1, POp(op2, a, b), c))
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
              let g = EFunction({def = [PVariable v, e2]; env = envi })
              in (matching (false, false) env f pf) @ (matching (false, false) env g pg)
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
  | (expr, PCompose(a, PCompose(b, c))) when not assoc_test ->
      begin
        try matching (com_test, true) env expr pattern 
        with MatchingFailure -> matching (com_test, true) env expr  (PCompose(PCompose(a, b), c))
      end
 | (expr, PCompose(PCompose(a, b), c)) when not assoc_test ->
      begin
        try matching (com_test, true) env expr pattern 
        with MatchingFailure -> matching (com_test, true) env expr (PCompose(a, PCompose(b, c)))
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


(** Evaluate top level definitions that change the environment globally.
   
     - [let ... = ...;;]
     - [declare ...;]
     - [open ...;]
*)
and eval env = function
  | ELet (def, None) ->
      let (env', _) = eval_definition env def
      in (env', EUnit)
  | EDeclare(var, None) ->
      let env' = add_env (var, (None, None)) env
      in (env', EUnit)
  | EOpen (m, None) -> 
      let env' = open_fun_module m env
      in (env', EUnit)
  | expr -> (env, eval' env expr)

(** Evaluate expressions that only change the environment locally.
   
    Recursively reduces an expression "as much as possible". Formal functions
    (see wiki) are reduced using the lambda calculus rules defined in
    {!Lambda_repl}.

    The builtin Prelude calls are also evaluated here.
    
    @see
    <https://github.com/robocop/CamlM/wiki/%C3%89l%C3%A9ments-de-s%C3%A9mantique-du-langage-CamlM>
    (in french) for details on formal functions.
*)
and eval' env expr = match expr with
  | EVariable s -> 
      begin  
        match lookup_env s env with
          | (Some e, _) -> e
          | (None, _) -> EVariable s (* If the variable was defined using declare, with no expression. *)
      end
  (* Only formal functions are reduced by lambda calculus rules. *)
  | EFunction {def = [PVariable v, expr]; env = None} -> 
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
          | EApplication(EVariable "^", ENum x), ENum y when (Int32.compare y Int32.zero) >= 0 -> ENum (puis x y)
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
          | EVariable "constant", x ->
            let vars = free_vars x in
            let var = new_variable vars "x" in 
            EFunction {def = [PVariable var, x]; env = Some env}
          | EFunction {def = def; env = Some env_f}, arg -> 
            (try
              eval_application env_f def arg
            with
                _  -> EApplication(f, e)
            )
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
      let env' = add_env (var, (None, None)) env
      in eval' env' corps
  | r -> r

(** Evaluate a function application by matching the argument with the function
    pattern. The environment is extended with the results from [matching] and the
    function call is evaluated within this environment.
  *)
and eval_application env case_list argument = match case_list with
  | [] -> raise (Error "Pattern matching failure")
  | (pattern, expr) :: rest ->
      try
        let extended_env = multi_add_env (matching (false, false) env argument pattern) env
        in eval' extended_env expr
      with
          MatchingFailure -> eval_application env rest argument

(** Evaluate a function definition. Builds a closure in the case of a
    recursive function, and uses the standard {!eval'} function otherwise.
  *)
and eval_definition curr_env def =
  match def.recursive with
    | false ->
        let value = eval' curr_env def.expr in
        let curr_env' = add_env (def.name, (Some value, None)) curr_env
        in (curr_env', value)
    | true -> (* Recursive function *)
        match def.expr with
          | EFunction f ->
              let closure = {def = f.def; env = None } in
              let new_entry = (def.name, (Some (EFunction closure), None)) in
              let extended_env = add_env new_entry curr_env in
                closure.env <- Some extended_env;
                (extended_env, EFunction closure)
          | _ -> raise (Error "Non-functionnal recursive let definition")

(** Evaluate a list of expressions, keeping the environment in between the
    calls.
  *)
and do_eval env = function
  | [] -> env
  | x :: xs -> 
      let (env', _) = eval env x
      in do_eval env' xs

(** See {!Modules.open_module} *)
and open_fun_module m env = open_module do_eval m env

