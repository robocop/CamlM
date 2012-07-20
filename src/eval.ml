open Syntax
open Modules
open Lambda_repl
open Error
open Helper

let value (_, v) = v
let env (e, _) = e


let is_com op env_ops = 
  try 
    List.mem Com (List.assoc op env_ops)
  with _ -> false
let is_assoc op env_ops = 
  try 
    List.mem Assoc (List.assoc op env_ops)
  with _ -> false


(* Fait correspondre une expression à un pattern, et calcule les nouvelles variables définies par le pattern  *)
(* Renvoit un bout d'environnement que l'on colle à l'environnement précédent                                 *)
(* On gère l'associativité et la commutativité des opérateurs grâce aux variables 
  booléeennes  (com_test, assoc_test) et à env_ops                                                            *)
let rec matching (com_test, assoc_test) (env, env_ops) value pattern = match value, pattern with
  | (_, PAll) -> []
  | (value, PAxiom id) ->
    begin
    try 
      (match value, List.assoc id env with
	| EVariable v, None when v = id -> []
	| _ -> raise MatchingFailure
      )
    with
      | Not_found -> raise (Error (id ^ "is not found"))
    end

  | (value, PVariable id) -> [id, Some value] 
  | (EBoolean b1, PBoolean b2) ->
      if b1 = b2 then [] else raise MatchingFailure
  | (ENum i1, PNum i2) ->
      if i1 = i2 then [] else raise MatchingFailure
  | (EString s1, PString s2) ->
      if s1 = s2 then [] else raise MatchingFailure
  | (EPair(v1, v2), PPair (m1, m2)) ->
      matching (false, false) (env, env_ops) v1 m1 @ matching (false, false) (env, env_ops) v2 m2
  | (ENil, PNil) -> []
  | (ECons (v1, v2), PCons(m1, m2)) ->
      matching (false, false) (env, env_ops) v1 m1 @ matching (false, false) (env, env_ops) v2 m2
  | (ENone, PNone) -> []
  | (ESome v, PSome m) -> matching (false, false) (env, env_ops) v m
  | (expr, POp(op1, POp(op2, a, b), c)) when op1 = op2 && is_assoc op1 env_ops && not assoc_test->
    begin
      try matching (com_test, true) (env, env_ops) expr pattern 
      with MatchingFailure -> matching (com_test, true) (env, env_ops) expr  (POp(op1, a, POp(op2, b, c)))
    end
  | (expr, POp(op, a, b)) when is_com op env_ops && not com_test ->
    (try matching (true, assoc_test) (env, env_ops) expr pattern 
    with MatchingFailure -> matching (true, assoc_test) (env, env_ops) expr (POp(op, b, a))
    )
  | (expr, POp(op, pf, pg)) ->
    begin
      match expr with
        | EFunction ({def = [PVariable v, EApplication(EApplication (EVariable op', e1), e2)]; env = envi}) when op = op' ->
          let f = EFunction({def = [PVariable v, e1]; env = envi }) in
	  let g = EFunction({def = [PVariable v, e2]; env = envi }) in
	  (matching (false, false) (env, env_ops) f pf) @ (matching (false, false) (env, env_ops) g pg)
        | _ -> raise MatchingFailure
    end 
     
  | (expr, PMinus m) ->
      (match expr with
         | EFunction
	     ({def = [PVariable v, EApplication (EVariable "-", e)]; 
	      env = envi}) ->
            let f' = EFunction({def = [PVariable v, e]; env = envi }) in
            matching (false, false) (env, env_ops) f' m
         | _ -> raise MatchingFailure
      )
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
           (matching (false, false) (env, env_ops) f pf) @ (matching (false, false) (env, env_ops) g pg)
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
	 else matching (false, false) (env, env_ops) e p
      | _ -> raise MatchingFailure
    end
  | (expr, PIsnum p) ->
    begin
      match expr with
	| ENum x -> matching (false, false) (env, env_ops) expr p
	| _ -> raise MatchingFailure
    end
  | (expr, PWhen(cond, p)) ->
    let r = matching (false, false) (env, env_ops) expr p in
    let env' = r @ env  in
    begin
      match eval (env', env_ops) cond with
	| _, (EBoolean true) -> r
	| _ -> raise MatchingFailure
    end
  | _ -> raise MatchingFailure



(* Top level definitions that change the env globally *)
and eval (env, env_ops) = function
  | ELet (def, None) ->
      let (env', _) = eval_definition (env, env_ops) def
      in (env', EUnit)
  | EDeclare(var, None) ->
    let env' = (var, None)::env in
    (env', EUnit)
  | EOpen (m, None) -> 
      let env' = open_module (env, env_ops) m
      in (env', EUnit)
  | expr -> (env, eval' (env, env_ops) expr)

(* Other expressions that only change the env locally *)
(* eval' réduit récursivement une expression 'le plus possible'  *)
(* Les fonctions formelles sont réduites à l'aide des règles de lambda calcul définies dans lambda_repl.ml *)
and eval' (env, env_ops) expr = match expr with
  | EVariable s -> 
      begin try 
	      (match List.assoc s env with
		| Some e -> e
		| None -> EVariable s (* Si la variable a été définie par la syntaxe 'declare' sans expression *)
	      )
	with _ -> raise (Error ("Unknown " ^ s)) 
      end
  | EFunction {def = [PVariable v, expr]; env = None}-> (* Seules les fonctions formelles sont réduites par lambda calcul *)
      let f = EFunction {def = [PVariable v, expr]; env = Some env} in
      if is_simple_value f then normal_order_reduct (replace env f)
      else f

  | EFunction {def = def; env = None} -> 
      EFunction {def = def; env = Some env}

  | EApplication(f, e) -> (* On evalue quelques primitives *)
    let f', x' = (eval' (env, env_ops) f, eval' (env, env_ops) e) in
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
              eval_application (env_f, env_ops) def arg
          | _ -> EApplication(f', x')
        end


  | EPair(e1, e2) -> EPair(eval' (env, env_ops) e1, eval' (env, env_ops) e2)
  | ECons(e1, e2) -> ECons(eval' (env, env_ops) e1, eval' (env, env_ops) e2)
  | ESome e -> ESome (eval' (env, env_ops) e)
  | EOpen (m, Some expr) ->
      let env' = open_module (env, env_ops) m
      in eval' (env', env_ops) expr
  | ELet(def, Some corps) ->
      eval' (fst (eval_definition (env, env_ops) def), env_ops) corps
  | EDeclare(var, Some corps) ->
    let env' = (var, None)::env in
    eval' (env', env_ops) corps
  | r -> r

and eval_application (env, env_ops) case_list argument = match case_list with
  | [] -> raise (Error "Pattern matching failure")
  | (pattern, expr) :: rest ->
      try
        let extended_env = matching (false, false) (env, env_ops) argument pattern @ env in
          eval' (extended_env, env_ops) expr
      with
          MatchingFailure -> eval_application (env, env_ops) rest argument

and eval_definition (curr_env, env_ops) def =
  match def.recursive with
    | false ->
        let value = eval' (curr_env, env_ops) def.expr
        in ((def.name, Some value) :: curr_env, value)
    | true -> (* Evaluation d'une définition récusive *)
        match def.expr with
          | EFunction f ->
              let closure = {def = f.def; env = None } in
              let new_entry = def.name, Some (EFunction closure) in
              let extended_env = new_entry :: curr_env in
                closure.env <- Some extended_env;
                (extended_env, EFunction closure)
          | _ -> raise (Error "Non-functionnal recursive let definition")

and do_eval (env, env_ops) = function
  | [] -> env
  | x :: xs -> 
      let (env', _) = eval (env, env_ops) x
      in do_eval (env', env_ops) xs

and open_module (env, env_ops) m = 
  let handle = open_in (file_from_module m) in
  let ast = parse Parser.file (Lexing.from_channel handle) 
  in close_in handle; do_eval (env, env_ops) ast

