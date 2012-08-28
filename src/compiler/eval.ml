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
open Show

let expr_property prop expr env = List.mem prop (expr_prop (lookup_env expr env))


let is_com op env = expr_property Com op env
let is_assoc op env = expr_property Assoc op env
let is_rec expr env = expr_property Recursive expr env

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
  | (value, PIsnum p) ->
    (match value with
      | ENum n -> matching (false, false) env (ENum n) p 
      | _ -> raise MatchingFailure
    )
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
        try matching (false, true) env expr pattern 
        with MatchingFailure -> matching (false, true) env expr  (POp(op1, a, POp(op2, b, c)))
      end
  | (expr, POp(op1, a, POp(op2, b, c))) when op1 = op2 && is_assoc op1 env && not assoc_test->
      begin
        try matching (false, true) env expr pattern 
        with MatchingFailure -> matching (false, true) env expr  (POp(op1, POp(op2, a, b), c))
      end
  | (expr, POp(op, a, b)) when is_com op env && not com_test ->
      (try matching (true, false) env expr pattern 
       with MatchingFailure -> matching (true, false) env expr (POp(op, b, a))
      )
  | (expr, POp(op, pf, pg)) ->
    begin
      match expr with
        | EApplication(EApplication (EVariable op', e1), e2) when op = op' ->
          (matching (false, false) env e1 pf) @ (matching (false, false) env e2 pg)
        | _ -> raise MatchingFailure
    end
  | (expr, PWhen(expr_cond, pattern)) -> 
    let vars = matching (false, false) env expr pattern in
    let r = List.fold_left 
        (fun expr (var, e_var) -> substitution expr (get (fst e_var)) var)
        expr_cond
        vars
    in
    (* Printf.printf "%s == %s ?\n" (show r) (show expr); *)
      (match eval' env r with
        | EBoolean true -> vars
        | _ -> raise MatchingFailure)
  | _ -> raise MatchingFailure


(** Evaluate top level definitions that change the environment globally.
   
     - [let ... = ...;;]
     - [declare ...;]
     - [open ...;]
*)
and eval env = function
  | ELet (def, None) ->
      let  (name, value, is_rec) = eval_definition env def in
      let value' = replace_env env value in
      let env' = add_env (name, (Some value', if is_rec then [Recursive] else [])) env in
      (env', EUnit)
 
  | EDeclare(var, None) ->
      let env' = add_env (var, (None, [])) env
      in (env', EUnit)
  | EOpen (m, None) -> 
      let env' = open_fun_module m env
      in (env', EUnit)
  | expr -> 
    let expr' = replace_env env expr in
    (env, eval' env expr')


and replace_env env expr = 
  let l = get_accessible_names env in
  List.fold_left
    (fun expr (name_var, (e_option, props)) ->
      match e_option with
	| None -> expr
	| Some value ->
	  if List.mem Recursive props then
	    let rec fixpoint f x = match f x with
              | x' when (x' = x) -> x
              | x' -> fixpoint f x'
	    in
	    let f expr = eval' env (substitution expr value name_var) in
	    fixpoint f expr
	  else 
	    let expr' = substitution expr value name_var in
	    eval' env expr'
	    	  
    )
    expr
    l
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
  | ELet(def, Some corps) ->
    let name_var, value, is_rec  = eval_definition env def in
    if not is_rec then 
      let corps' = substitution corps value name_var in
      eval' env corps'
    else 
      let rec fixpoint f x = match f x with
        | x' when (x' = x) -> x
        | x' -> fixpoint f x'
      in
      let f expr = eval' env (substitution expr value name_var) in
      fixpoint f corps

  | EApplication (EFunction {def = case_list; n = name} as f, argument) ->
    let arg = eval' env argument in
    begin try
    let new_vars, f_x = eval_application env case_list arg in
    let r = List.fold_left 
      (fun f_x (var, e_var) -> substitution f_x (get (fst e_var)) var)
      f_x 
      new_vars
    in
    eval' env r
      with _ -> 
	(match name with 
	    Some n -> EApplication(EVariable n, argument)
	  | None -> EApplication(f, argument)
	)
    end

  | EApplication(m, n) as r  -> 
    let r' = primitive (eval' env m) (eval' env n) in
    if r' <> r then eval' env r' else r'
  | EFunction {def = case_list; n = n } ->
    EFunction {def = List.map (fun (p, e) -> (p, eval' env e)) case_list;  n = n}
  

  | EDeclare(_, Some expr) -> eval' env expr

  | EPair(a, b) -> EPair(eval' env a, eval' env b)
  | ECons(a, b) -> ECons(eval' env a, eval' env b)
  | ESome(e) -> ESome(eval' env e)
  | e -> e

and primitive f' x' = match (f', x') with
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
  | f, x -> EApplication(f, x)

(** Evaluate a function application by matching the argument with the function
    pattern. The environment is extended with the results from [matching] and the
    function call is evaluated within this environment.
  *)
and eval_application env case_list argument = match case_list with
  | [] -> raise MatchingFailure
  | (pattern, expr) :: rest ->
      try
        (matching (false, false) env argument pattern, expr)
      with
          MatchingFailure -> eval_application env rest argument

(** Evaluate a function definition. Builds a closure in the case of a
    recursive function, and uses the standard {!eval'} function otherwise.
  *)
and eval_definition curr_env def =
  match def.expr, def.recursive with
    | EFunction f, r -> 
       let closure = {def = f.def; n = Some def.name } in
       (def.name, EFunction closure, r)
    | _, true ->  raise (Error "Non-functionnal recursive let definition")
    | expr, _ -> 
      let value = eval' curr_env def.expr in
      (def.name, value, false)

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

