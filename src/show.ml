open Syntax

let rec print_list = function
  | [] -> print_string "[]"
  | x::xs -> print_string (x^"::"); print_list xs

let rec print_def def = 
  Printf.sprintf "let %s%s = %s" 
    (if def.recursive then "rec " else "") 
    def.name 
    (imprime def.expr)

and print_motif = function
  | PAll -> "_"
  | PVariable s -> s
  | PBoolean b -> Printf.sprintf "%b" b;
  | PNum n -> string_of_int n
  | PPair (e1, e2) -> Printf.sprintf "(%s,%s)" (print_motif e1) (print_motif e2)
  | PNil -> "[]"
  | PCons (e1, e2) -> Printf.sprintf "%s::%s" (print_motif e1) (print_motif e2) 
  | PNone -> "None"
  | PSome m -> Printf.sprintf "Some %s" (print_motif m)
  | PString s -> Printf.sprintf "\"%s\"" s
  | FunP_op (op, m1, m2) -> Printf.sprintf "%s %s %s" (print_motif m1) op (print_motif m2)
  | FunP_m m -> Printf.sprintf "-%s" (print_motif m)
  | FunP_id -> "Id"
  | FunP_const m -> Printf.sprintf "Const %s" (print_motif m)

and print_fonction def = 
  "function\n" ^ (String.concat "| " (List.map (fun (m, e) -> Printf.sprintf "%s -> %s\n" (print_motif m) (imprime e)) def))

and imprime = function
  | EVariable v -> v
  | ENum n -> string_of_int n
  | EString s -> Printf.sprintf "\"%s\"" s
  | EBoolean false -> "false"
  | EBoolean true -> "true"
  | EPair (e1, e2) ->
      "("^imprime e1^", "^imprime e2^")"
  | ENil -> "[]"
  | EUnit -> "()"
  | EOpen (m, Some expr) -> "open " ^ m ^ " in " ^ imprime expr
  | EOpen (m, None) -> "open " ^ m
  | ECons(e1, e2) ->
      imprime e1 ^ "::" ^imprime e2
  | EApplication (EApplication (EVariable op, e1), e2) 
      when List.mem op ["+"; "*"; "/"] -> 
      Printf.sprintf "(%s %s %s)" (imprime e1) op (imprime e2)
  | EApplication (f, e) ->
      "("^imprime f^") "^"("^imprime e^")" 
  | EFunction {def = def; env = _} -> 
      begin
        match def with
          | [PVariable v, expr] ->
              Printf.sprintf "\\%s -> %s" v  (imprime expr)
          | _ -> print_fonction def
      end
  | ENone -> "None"
  | ESome e -> Printf.sprintf "Some %s" (imprime e) 
  | ELet (def, Some expr) ->
      (print_def def) ^ " in " ^ imprime expr
  | ELet (def, None) -> print_def def


