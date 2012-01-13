open Syntax
open Modules
open Lambda_repl
open Error
open Helper

let scope : (string * expression) list ref = ref []

let rec filtrage valeur motif = match valeur, motif with
  | (_, PAll) -> []
  | (valeur, PVariable id) -> [id, valeur]
  | (EBoolean b1, PBoolean b2) ->
      if b1 = b2 then [] else raise MatchingFailure
  | (ENum i1, PNum i2) ->
      if i1 = i2 then [] else raise MatchingFailure
  | (EString s1, PString s2) ->
      if s1 = s2 then [] else raise MatchingFailure
  | (EPair(v1, v2), PPair (m1, m2)) ->
      filtrage v1 m1 @ filtrage v2 m2
  | (ENil, PNil) -> []
  | (ECons (v1, v2), PCons(m1, m2)) ->
      filtrage v1 m1 @ filtrage v2 m2
  | (ENone, PNone) -> []
  | (ESome v, PSome m) -> filtrage v m
  | (f, FunP_const m) ->
      (match const f with
         | Some v -> filtrage v m
         | None -> raise MatchingFailure
      )
  | (f, FunP_op(op, m1, m2)) ->
      (match decompose_op op f with
         | Some (f1, f2) ->
             (filtrage f1 m1) @ (filtrage f2 m2)
         | None -> raise MatchingFailure
      )
  | (f, FunP_m m) ->
      (match minus f with
         | Some f -> filtrage f m
         | None -> raise MatchingFailure
      )
  | (f, FunP_id) ->
      if is_id f then []
      else raise MatchingFailure
  | _ -> raise MatchingFailure

let value (_, v) = v
let env (e, _) = e

(* Top level definitions that change the env globally *)
let rec evalue env = function
  | ELet (def, None) ->
      let (env', _) = evalue_definition env def
      in (env', EUnit)
  | EOpen (m, None) -> 
      let env' = open_module env m
      in (env', EUnit)
  | expr -> (env, evalue' env expr)

(* Other expressions that only change the env locally *)
and evalue' env expr = match expr with
  | EVariable s ->
      begin try List.assoc s env with _ -> raise (Error ("Unknown " ^ s)) end

  | EFunction {def = [PVariable v, expr]; env = None}->
      let f = EFunction {def = [PVariable v, expr]; env = Some env} in
        normal_order_reduct (replace env f)

  | EFunction {def = def; env = None} -> 
      EFunction {def = def; env = Some env}

  | EApplication(f, e) ->
      let eval_f = evalue' env f in
      let eval_e = evalue' env e in
        begin match eval_f with
          | EPrimitive f -> f eval_e
          | EFunction {def = def; env = Some env_f} -> 
              evalue_application env_f def eval_e
          | _ -> raise (Error "application d'une valeur non fonctionelle")
        end
  | EPair(e1, e2) -> EPair(evalue' env e1, evalue' env e2)
  | ECons(e1, e2) -> ECons(evalue' env e1, evalue' env e2)
  | ESome e -> ESome (evalue' env e)
  | EOpen (m, Some expr) ->
      let env' = open_module env m
      in evalue' env' expr
  | ELet(def, Some corps) ->
      evalue' (fst (evalue_definition env def)) corps
  | r -> r

and evalue_application env list_de_cas argument = match list_de_cas with
  | [] -> raise (Error "echec du filtrage")
  | (motif, expr) :: autres_cas ->
      try
        let env_etendu = filtrage argument motif @ env in
          evalue' env_etendu expr
      with
          MatchingFailure -> evalue_application env autres_cas argument

and evalue_definition env_courant def =
  match def.recursive with
    | false ->
        let valeur = evalue' env_courant def.expr
        in ((def.name, valeur)::env_courant, valeur)
    | true ->
        match def.expr with
          | EFunction ferm ->
              let fermeture = {def = ferm.def; env = None } in
              let env_etendu = (def.name, EFunction fermeture)::env_courant in
                fermeture.env <- Some env_etendu;
                (env_etendu, EFunction fermeture)
          | _ -> raise (Error "let rec non fonctionnel") 

and do_eval env = function
  | [] -> env
  | x :: xs -> 
      let (env', _) = evalue env x
      in do_eval env' xs

and open_module env m = 
  let handle = open_in (file_from_module m) in
  let ast = parse Parser.file (Lexing.from_channel handle) 
  in close_in handle; do_eval env ast

