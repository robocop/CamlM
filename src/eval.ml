open Syntax
open Modules
open Lambda_repl
open Error
open Helper

let rec matching value pattern = match value, pattern with
  | (_, PAll) -> []
  | (value, PVariable id) -> [id, value]
  | (EBoolean b1, PBoolean b2) ->
      if b1 = b2 then [] else raise MatchingFailure
  | (ENum i1, PNum i2) ->
      if i1 = i2 then [] else raise MatchingFailure
  | (EString s1, PString s2) ->
      if s1 = s2 then [] else raise MatchingFailure
  | (EPair(v1, v2), PPair (m1, m2)) ->
      matching v1 m1 @ matching v2 m2
  | (ENil, PNil) -> []
  | (ECons (v1, v2), PCons(m1, m2)) ->
      matching v1 m1 @ matching v2 m2
  | (ENone, PNone) -> []
  | (ESome v, PSome m) -> matching v m

  | (expr, POp(op, m1, m2)) ->
      (match decompose_op op expr with
         | Some (e1, e2) ->
             (matching e1 m1) @ (matching e2 m2)
         | None -> raise MatchingFailure
      )
  | (expr, PMinus m) ->
      (match minus expr with
         | Some expr -> matching expr m
         | None -> raise MatchingFailure
      )
  | _ -> raise MatchingFailure

let value (_, v) = v
let env (e, _) = e

(* Top level definitions that change the env globally *)
let rec eval env = function
  | ELet (def, None) ->
      let (env', _) = eval_definition env def
      in (env', EUnit)
  | EOpen (m, None) -> 
      let env' = open_module m
      in (env', EUnit)
  | expr -> (env, eval' env expr)

(* Other expressions that only change the env locally *)
and eval' env expr = match expr with
  | EVariable "/" -> EVariable "/"
  | EVariable s -> 
      begin try List.assoc s env with _ -> raise (Error ("Unknown " ^ s)) end

  | EFunction {def = [PVariable v, expr]; env = None}->
      let f = EFunction {def = [PVariable v, expr]; env = Some env} in
        normal_order_reduct (replace env f)

  | EFunction {def = def; env = None} -> 
      EFunction {def = def; env = Some env}

  | EApplication(f, e) ->
      let eval_f = eval' env f in
      let eval_e = eval' env e in
        begin match eval_f with
          | EPrimitive f -> f eval_e
          | EFunction {def = def; env = Some env_f} -> 
              eval_application env_f def eval_e
          | _ -> (*raise (Error "Cannot apply non-functionnal expression") *) EApplication(eval_f, eval_e)
        end
  | EPair(e1, e2) -> EPair(eval' env e1, eval' env e2)
  | ECons(e1, e2) -> ECons(eval' env e1, eval' env e2)
  | ESome e -> ESome (eval' env e)
  | EOpen (m, Some expr) ->
      let env' = open_module m
      in eval' (env' @ env) expr
  | ELet(def, Some corps) ->
      eval' (fst (eval_definition env def)) corps
  | r -> r

and eval_application env case_list argument = match case_list with
  | [] -> raise (Error "Pattern matching failure")
  | (pattern, expr) :: rest ->
      try
        let extended_env = matching argument pattern @ env in
          eval' extended_env expr
      with
          MatchingFailure -> eval_application env rest argument

and eval_definition curr_env def =
  match def.recursive with
    | false ->
        let valeur = eval' curr_env def.expr
        in ((def.name, valeur) :: curr_env, valeur)
    | true ->
        match def.expr with
          | EFunction f ->
              let closure = {def = f.def; env = None } in
              let new_entry = def.name, EFunction closure in
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

