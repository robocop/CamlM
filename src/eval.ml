open Syntaxe

let scope = ref []

let rec imprime = function
  | Nombre n -> string_of_int n
  | String s -> Printf.sprintf "\"%s\"" s
  | Booleen false -> "false"
  | Booleen true -> "true"
  | Paire(e1, e2) ->
    "("^imprime e1^", "^imprime e2^")"
  | Val_nuple vs ->
    "(" 
    ^ List.fold_left (fun acc x -> acc ^ ", " ^ imprime x) 
        (imprime (List.hd vs))
        (List.tl vs)
    ^ ")"
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
;;
