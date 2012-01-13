open Syntaxe

let rec print_list = function
  | [] -> print_string "[]"
  | x::xs -> print_string (x^"::"); print_list xs

let rec print_def def = 
  Printf.sprintf "let %s%s = %s" 
    (if def.recursive then "rec " else "") 
    def.nom 
    (imprime def.expr)

and print_motif = function
  | Motif_all -> "_"
  | Motif_variable s -> s
  | Motif_booleen b -> Printf.sprintf "%b" b;
  | Motif_nombre n -> string_of_int n
  | Motif_paire (e1, e2) -> Printf.sprintf "(%s,%s)" (print_motif e1) (print_motif e2)
  | Motif_nil -> "[]"
  | Motif_cons (e1, e2) -> Printf.sprintf "%s::%s" (print_motif e1) (print_motif e2) 
  | Motif_none -> "None"
  | Motif_some m -> Printf.sprintf "Some %s" (print_motif m)
  | Motif_string s -> Printf.sprintf "\"%s\"" s
  | FMotif_op (op, m1, m2) -> Printf.sprintf "%s %s %s" (print_motif m1) op (print_motif m2)
  | FMotif_m m -> Printf.sprintf "-%s" (print_motif m)
  | FMotif_Id -> "Id"
  | FMotif_const m -> Printf.sprintf "Const %s" (print_motif m)

and print_fonction def = 
  "fonction\n" ^ (String.concat "| " (List.map (fun (m, e) -> Printf.sprintf "%s -> %s\n" (print_motif m) (imprime e)) def))

and imprime = function
  | Variable v -> v
  | Nombre n -> string_of_int n
  | String s -> Printf.sprintf "\"%s\"" s
  | Booleen false -> "false"
  | Booleen true -> "true"
  | Paire(e1, e2) ->
      "("^imprime e1^", "^imprime e2^")"
  | Nil -> "[]"
  | Unit -> "()"
  | Open (m, Some expr) -> "open " ^ m ^ " in " ^ imprime expr
  | Open (m, None) -> "open " ^ m
  | Cons(e1, e2) ->
      imprime e1 ^ "::" ^imprime e2
  | Application (Application (Variable op, e1), e2) 
      when List.mem op ["+"; "*"; "/"] -> 
      Printf.sprintf "(%s %s %s)" (imprime e1) op (imprime e2)
  | Application(f, e) ->
      "("^imprime f^") "^"("^imprime e^")" 
  | Fonction {def = def; environnement = _} -> 
      begin
        match def with
          | [Motif_variable v, expr] ->
              Printf.sprintf "\\%s -> %s" v  (imprime expr)
          | _ -> print_fonction def
      end
  | CNone -> "None"
  | CSome e -> Printf.sprintf "Some %s" (imprime e) 
  | Let(def, Some expr) ->
      (print_def def) ^ " in " ^ imprime expr
  | Let(def, None) -> print_def def


