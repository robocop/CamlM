(** Lambda calculus reduction rules applied to formal functions.
   
   @see
   <https://github.com/robocop/CamlM/wiki/%C3%89l%C3%A9ments-de-s%C3%A9mantique-du-langage-CamlM>
   (in french) for details on formal functions.
*)

open Syntax
open Error
open Helper
open Modules
open Show

(** Recursively defines the expressions to be reduced by the lambda calculus
    rules. 
*)

let rec get_vars_of_pattern = function
  | PVariable s -> StringSet.singleton s
  | PPair(a, b) | PCons(a, b) | POp(_, a, b) ->
    StringSet.union (get_vars_of_pattern a) (get_vars_of_pattern b)
  | PSome a -> get_vars_of_pattern a
  | PWhen(_, p) -> get_vars_of_pattern p
  | _ -> StringSet.empty

(** Creates a set of all free variables of a function. *)
let rec free_vars = function
  | EVariable x -> StringSet.singleton x
  | EFunction {def = l} ->
    List.fold_left
      (fun ens (pattern, expr) -> 
        (match pattern with
          | PWhen(cond_expr, p) -> 
            let e = StringSet.diff (StringSet.union (free_vars expr) (free_vars cond_expr)) (get_vars_of_pattern pattern) in
            StringSet.union ens e
          | _ ->
            let e = StringSet.diff (free_vars expr) (get_vars_of_pattern pattern) in
            StringSet.union ens e
        )
      )
      StringSet.empty
      l
  | EApplication(m, n) | EPair(m, n) | ECons(m, n) 
  | ELet({expr = m}, Some n)
    ->
      StringSet.union (free_vars m) (free_vars n)
  | ESome n | EDeclare(_, Some n) | EOpen(_, Some n) ->
    free_vars n
  | _ -> StringSet.empty


(** Rename the variable x by z in the pattern*)
let rec rename_a_pattern x z = function
  | PVariable y when y = x -> PVariable z
  | PVariable y -> PVariable y
  | PPair(a, b) -> PPair(rename_a_pattern x z a, rename_a_pattern x z b)
  | PCons(a, b) -> PCons(rename_a_pattern x z a, rename_a_pattern x z b)
  | POp(op, a, b) -> POp(op, rename_a_pattern x z a, rename_a_pattern x z b)
  | PSome a -> PSome(rename_a_pattern x z a)
  | PWhen(expr, p) -> PWhen(expr, rename_a_pattern x z p) 
(* .. *)
  | r -> r

(** Substitutes a variable [x] for an expression [arg] in [expr] using lambda
    calculus rules.
  *)

let rec substitution expr arg x = match expr with
  | EVariable v when v = x -> arg
  | EVariable y when y <> x -> EVariable y


  | EFunction {def = l; n = n} ->
    let vars_arg = free_vars arg in
    let new_def =
      List.map (fun (pattern, expr) -> (* Pour chaque couple pattern * expression de la fonction *)
      let vars_p = get_vars_of_pattern pattern in (* On calcule les variables du pattern *)
      let (p, e) = StringSet.fold 
        (fun y (pattern, expr) -> (* Pour chacunes de ces variables y *)  
          (match pattern with
            | PWhen(expr_cond, p) ->
              let ens =  StringSet.union 
                (StringSet.union vars_arg (StringSet.diff (StringSet.union (free_vars expr) (free_vars expr_cond)) vars_p)) 
                (StringSet.singleton x) 
              in
              let z = new_variable ens y in
              let p' = rename_a_pattern y z p in
              let expr' = substitution expr (EVariable z) y in
              let expr_cond' = substitution expr_cond (EVariable z) y in
              let expr_cond2' = substitution expr_cond' arg x in
              let pattern' = PWhen(expr_cond2', p') in
              (pattern', expr')
            | _ ->
              let ens =  StringSet.union 
                (StringSet.union vars_arg (StringSet.diff (free_vars expr) vars_p)) 
                (StringSet.singleton x) 
              in
              let z = new_variable ens y in
              let pattern' = rename_a_pattern y z pattern in
              let expr' = substitution expr (EVariable z) y in
              (pattern', expr')
          )
        ) 
        vars_p
        (pattern, expr)
      in
      (p, substitution e arg x)
    )
      l
    in
    EFunction{def = new_def; n = n}

(*
  | EFunction {def = [PVariable v, e]; env = env} when v = x->
    EFunction {def = [PVariable v, e]; env = env}
  | EFunction {def = [PVariable y, e]; env = env} as f when y <> x ->
    let ens = StringSet.union (StringSet.union (free_vars arg) (free_vars f)) (StringSet.singleton x) in
    let z = new_variable ens y in
    let e1 = substitution e (EVariable z) y in
    let e2 = substitution e1 arg x in
    EFunction {def = [PVariable z, e2]; env = env}
*)

  | ELet({name = n; expr = a} as def, Some b) when n <> x ->
    ELet({def with expr = substitution a arg x}, Some (substitution b arg x))
  | ELet({expr = a} as def, Some b)  ->
    ELet({def with expr = substitution a arg x}, Some b)


  | EDeclare(s, Some expr) -> EDeclare(s, Some (substitution expr arg x))
  | EOpen(m, Some expr) -> EOpen(m, Some (substitution expr arg x))

  | EApplication(n1, n2) -> 
    EApplication(substitution n1 arg x, substitution n2 arg x)
  | EPair(n1, n2) -> EPair(substitution n1 arg x, substitution n2 arg x)
  | ECons(n1, n2) -> ECons(substitution n1 arg x, substitution n2 arg x)
  | ESome(n) -> ESome(substitution n arg x)
  | rest -> rest
