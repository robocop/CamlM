open Syntaxe
open Modules
open Lambda_repl
open Error
open Helper

let scope : (string * expression) list ref = ref []

let rec filtrage valeur motif = match valeur, motif with
  | (_, Motif_all) -> []
  | (valeur, Motif_variable id) -> [id, valeur]
  | (Booleen b1, Motif_booleen b2) ->
      if b1 = b2 then [] else raise Echec_filtrage
  | (Nombre i1, Motif_nombre i2) ->
      if i1 = i2 then [] else raise Echec_filtrage
  | (String s1, Motif_string s2) ->
      if s1 = s2 then [] else raise Echec_filtrage
  | (Paire(v1, v2), Motif_paire (m1, m2)) ->
      filtrage v1 m1 @ filtrage v2 m2
  | (Nil, Motif_nil) -> []
  | (Cons (v1, v2), Motif_cons(m1, m2)) ->
      filtrage v1 m1 @ filtrage v2 m2
  | (CNone, Motif_none) -> []
  | (CSome v, Motif_some m) -> filtrage v m
  | (f, FMotif_const m) ->
      (match const f with
         | Some v -> filtrage v m
         | None -> raise Echec_filtrage
      )
  | (f, FMotif_op(op, m1, m2)) ->
      (match decompose_op op f with
         | Some (f1, f2) ->
             (filtrage f1 m1) @ (filtrage f2 m2)
         | None -> raise Echec_filtrage
      )
  | (f, FMotif_m m) ->
      (match minus f with
         | Some f -> filtrage f m
         | None -> raise Echec_filtrage
      )
  | (f, FMotif_Id) ->
      if is_id f then []
      else raise Echec_filtrage
  | _ -> raise Echec_filtrage

let value (_, v) = v
let env (e, _) = e

(* Top level definitions that change the env globally *)
let rec evalue env = function
  | Let (def, None) ->
      let (env', _) = evalue_definition env def
      in (env', Unit)
  | Open (m, None) -> 
      let env' = open_module env m
      in (env', Unit)
  | expr -> (env, evalue' env expr)

(* Other expressions that only change the env locally *)
and evalue' env expr = match expr with
  | Variable s ->
      begin try List.assoc s env with _ -> raise (Erreur (s ^ " non connu")) end

  | Fonction {def = [Motif_variable v, expr]; environnement = None}->
      let f = Fonction {def = [Motif_variable v, expr]; environnement = Some env} in
        normal_order_reduct (replace env f)

  | Fonction {def = def; environnement = None} -> 
      Fonction {def = def; environnement = Some env}

  | Application(f, e) ->
      let eval_f = evalue' env f in
      let eval_e = evalue' env e in
        begin match eval_f with
          | Primitive f -> f eval_e
          | Fonction {def = def; environnement = Some env_f} -> 
              evalue_application env_f def eval_e
          | _ -> raise (Erreur "application d'une valeur non fonctionelle")
        end
  | Paire(e1, e2) -> Paire(evalue' env e1, evalue' env e2)
  | Cons(e1, e2) -> Cons(evalue' env e1, evalue' env e2)
  | CSome e -> CSome (evalue' env e)
  | Open (m, Some expr) ->
      let env' = open_module env m
      in evalue' env' expr
  | Let(def, Some corps) ->
      evalue' (fst (evalue_definition env def)) corps
  | r -> r

and evalue_application env list_de_cas argument = match list_de_cas with
  | [] -> raise (Erreur "echec du filtrage")
  | (motif, expr) :: autres_cas ->
      try
        let env_etendu = filtrage argument motif @ env in
          evalue' env_etendu expr
      with
          Echec_filtrage -> evalue_application env autres_cas argument

and evalue_definition env_courant def =
  match def.recursive with
    | false ->
        let valeur = evalue' env_courant def.expr
        in ((def.nom, valeur)::env_courant, valeur)
    | true ->
        match def.expr with
          | Fonction ferm ->
              let fermeture = {def = ferm.def; environnement = None } in
              let env_etendu = (def.nom, Fonction fermeture)::env_courant in
                fermeture.environnement <- Some env_etendu;
                (env_etendu, Fonction fermeture)
          | _ -> raise (Erreur "let rec non fonctionnel") 

and do_eval env = function
  | [] -> env
  | x :: xs -> 
      let (env', _) = evalue env x
      in do_eval env' xs

and open_module env m = 
  let handle = open_in (file_from_module m) in
  let ast = parse Parser.file (Lexing.from_channel handle) 
  in close_in handle; do_eval env ast

