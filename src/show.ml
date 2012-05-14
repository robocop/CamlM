open Syntax

let rec show_def def = 
  Printf.sprintf "let %s%s = %s" 
    (if def.recursive then "rec " else "") 
    def.name 
    (show def.expr)

and show_pattern = function
  | PAll -> "_"
  | PVariable s -> s
  | PBoolean b -> Printf.sprintf "%b" b;
  | PNum n -> string_of_int n
  | PPair (e1, e2) -> Printf.sprintf "(%s,%s)" (show_pattern e1) (show_pattern e2)
  | PNil -> "[]"
  | PCons (e1, e2) -> Printf.sprintf "%s::%s" (show_pattern e1) (show_pattern e2) 
  | PNone -> "None"
  | PSome m -> Printf.sprintf "Some %s" (show_pattern m)
  | PString s -> Printf.sprintf "\"%s\"" s
  | POp (op, m1, m2) -> Printf.sprintf "%s %s %s" (show_pattern m1) op (show_pattern m2)
  | PMinus m -> Printf.sprintf "-%s" (show_pattern m)

  | PApplication (PVariable f, PVariable x) -> Printf.sprintf "%s %s" f x
  | PApplication (PVariable s, PNum x) -> Printf.sprintf "%s %d" s x
  | PApplication (f, x) ->
     "("^show_pattern f^") "^"("^show_pattern x^")" 
  | PFunction(var, pexpr) ->
    Printf.sprintf "(\\%s -> %s)" var (show_pattern pexpr)

    

and show_function def = 
  "function\n" 
    ^ (String.concat "| " (List.map 
        (fun (m, e) -> 
           Printf.sprintf "%s -> %s\n" (show_pattern m) (show e)) def))

and show = function
  | EVariable v -> v
  | ENum n -> string_of_int n
  | EString s -> Printf.sprintf "\"%s\"" s
  | EBoolean false -> "false"
  | EBoolean true -> "true"
  | EPair (e1, e2) ->
      "("^show e1^", "^show e2^")"
  | ENil -> "[]"
  | EUnit -> "()"
  | EOpen (m, Some expr) -> "open " ^ m ^ " in " ^ show expr
  | EOpen (m, None) -> "open " ^ m
  | ECons(e1, e2) ->
      show e1 ^ "::" ^show e2
  | EApplication (EApplication (EVariable op, e1), e2) 
      when List.mem op ["+"; "*"; "/"] -> 
      Printf.sprintf "(%s %s %s)" (show e1) op (show e2)
  | EApplication (EVariable f, EVariable x) -> Printf.sprintf "%s %s" f x
  | EApplication (EVariable s, ENum x) -> Printf.sprintf "%s %d" s x
  | EApplication (f, e) ->
      "("^show f^") "^"("^show e^")" 
  | EFunction {def = def; env = _} -> 
      begin
        match def with
          | [PVariable v, expr] ->
              Printf.sprintf "\\%s -> %s" v  (show expr)
          | _ -> show_function def
      end
  | ENone -> "None"
  | ESome e -> Printf.sprintf "Some %s" (show e) 
  | ELet (def, Some expr) ->
      (show_def def) ^ " in " ^ show expr
  | ELet (def, None) -> show_def def


