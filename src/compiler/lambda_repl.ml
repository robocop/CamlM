(** Lambda calculus reduction rules extended to the complete caml syntax.
   *)
open Syntax
open Error
open Helper
open Modules
open Show

(** This function compute the variables of a pattern.
    For exemple, in the pattern (a, _+b), get_vars_of_pattern = {a, b}
    (return a set)
*)
let rec get_vars_of_pattern = function
  | PVariable s -> StringSet.singleton s
  | PPair(a, b) | PCons(a, b) | POp(_, a, b) | PApplication(a, b) ->
    StringSet.union (get_vars_of_pattern a) (get_vars_of_pattern b)
  | PSome a | PMinus a | PWhen(_, a) -> get_vars_of_pattern a
  | _ -> StringSet.empty

(** Creates a set of all free variables of an expression (return a set)
    For exemple, in the expression "\x -> y+x", free_vars = {y}.
    (return a set)
*)
let rec free_vars = function
  | EVariable x -> StringSet.singleton x
  | EFunction {def = l} -> (** We extended the rule FV(\x -> expr) = FV(expr) - {x} to all the functions (with get_vars_of_pattern) *)
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
  | ELet({expr = m}, Some n)  -> StringSet.union (free_vars m) (free_vars n)
  | ESome n | EDeclare(_, Some n) | EOpen(_, Some n) ->
    free_vars n
  | _ -> StringSet.empty


(** Rename the variable x by z in a pattern*)
let rec rename_a_pattern x z = function
  | PVariable y when y = x -> PVariable z
  | PVariable y -> PVariable y
  | PPair(a, b) -> PPair(rename_a_pattern x z a, rename_a_pattern x z b)
  | PCons(a, b) -> PCons(rename_a_pattern x z a, rename_a_pattern x z b)
  | POp(op, a, b) -> POp(op, rename_a_pattern x z a, rename_a_pattern x z b)
  | PMinus(a) -> PMinus(rename_a_pattern x z a)
  | PApplication(a, b) -> PApplication(rename_a_pattern x z a, rename_a_pattern x z b)
  | PSome a -> PSome(rename_a_pattern x z a)
  | PWhen(expr, p) -> PWhen(expr, rename_a_pattern x z p) 
  | r -> r

(** Substitutes a variable [x] for an expression [arg] in [expr] using lambda
    calculus rules (extended to support all the caml's expressions)
    We note "substitution expr arg x =  expr[x := arg]"
  *)

let rec substitution expr arg x = match expr with
  | EVariable v when v = x -> arg (** v[v := arg] = arg *)
  | EVariable y when y <> x -> EVariable y (** y[v:=arg] = y if y <> v *)


  | EFunction {def = l; n = n} -> 
    (** We extended the rule 
	(\y -> e)[x:=arg] = (\z -> e2) where z is not in FV(arg) U free_vars (\y -> e) U {x} and where e2 is (e[y:=z])[x:=arg]
	if y = x, (\y -> e)[x:=arg] is also (\y -> e)
	We use rename_a_pattern
    *)
    let vars_arg = free_vars arg in
    let new_def =
      List.map (fun (pattern, expr) -> (** For eatch couple of pattern * expression of the function *)
      let vars_p = get_vars_of_pattern pattern in (* We compute the pattern's variables *)
      let (p, e) = StringSet.fold 
        (fun y (pattern, expr) -> (** For eatch of these variables y *)  
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
              let ens =  StringSet.union  (** We compute ens = FV(arg) U free_vars (\y -> e) U {x} *)
                (StringSet.union vars_arg (StringSet.diff (free_vars expr) vars_p)) 
                (StringSet.singleton x) 
              in
              let z = new_variable ens y in (** We generate a new variable z, where z is not in ens *)
              let pattern' = rename_a_pattern y z pattern (** We rename the pattern with the variable z *)in
              let expr' = substitution expr (EVariable z) y (** And we replace y by z in the expression associed to the pattern *)in
              (pattern', expr')
          )
        ) 
        vars_p
        (pattern, expr)
      in
      (** Once all the pattern's variables are replaced, we can replace in e x by arg *)
      (p, substitution e arg x)
    )
      l
    in
    EFunction{def = new_def; n = n}

  | ELet({name = n; expr = a} as def, Some b) when n <> x -> (* if we introduce a new variable n, n <> x *)
    ELet({def with expr = substitution a arg x}, Some (substitution b arg x))
  | ELet({expr = a; recursive = false} as def, Some b)  -> (* if we introduce a (nonrecursive) variable n where n = x *)
    ELet({def with expr = substitution a arg x}, Some b)

  | EDeclare(s, Some expr) -> EDeclare(s, Some (substitution expr arg x))
  | EOpen(m, Some expr) -> EOpen(m, Some (substitution expr arg x))

  | EApplication(n1, n2) -> (* (a b)[x:=arg] is a[x:=arg] b[x:=arg] *)
    EApplication(substitution n1 arg x, substitution n2 arg x)

  | EPair(n1, n2) -> EPair(substitution n1 arg x, substitution n2 arg x)
  | ECons(n1, n2) -> ECons(substitution n1 arg x, substitution n2 arg x)
  | ESome(n) -> ESome(substitution n arg x)
  | rest -> rest
