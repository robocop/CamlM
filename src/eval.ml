open Syntaxe

(* let scope = ref [] *)

let rec print_def def = 
  Printf.sprintf "let %s%s = %s" 
    (if def.recursive then "rec " else "") 
    def.nom 
    (imprime def.expr)
and imprime = function
  | Variable v -> v
  | Nombre n -> string_of_int n
  | String s -> Printf.sprintf "\"%s\"" s
  | Booleen false -> "false"
  | Booleen true -> "true"
  | Paire(e1, e2) ->
    "("^imprime e1^", "^imprime e2^")"
  | Nil -> "[]"
  | Cons(e1, e2) ->
    imprime e1 ^ "::" ^imprime e2
  | Application(f, e) ->
    "("^imprime f^") "^"("^imprime e^")" 
  | Fonction (def, env) -> 
    begin
      match def with
	| [Motif_variable v, expr] ->
	   Printf.sprintf "\\%s -> %s" v  (imprime expr)
	| _ -> "<fun>"
    end
  | CNone -> "None"
  | CSome e -> Printf.sprintf "Some %s" (imprime e) 
  | Let(def, Some expr) ->
    (print_def def) ^ " in " ^ imprime expr
  | Let(def, None) -> print_def def
;;
