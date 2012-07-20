open Syntax

(* Affichage des expressions réduites par eval    *)
(* N'affiche pas les environnements des fonctions *)

let show_list l = 
  "["^(String.concat "; " l)^"]"


let rec show_def def = 
  Printf.sprintf "let %s%s = %s" 
    (if def.recursive then "rec " else "") 
    def.name 
    (show def.expr)

and show_pattern = function
  | PAll -> "_"
  | PVariable s -> s
  | PAxiom s -> "@"^s
  | PBoolean b -> Printf.sprintf "%b" b;
  | PNum n -> Int32.to_string n
  | PPair (e1, e2) -> Printf.sprintf "(%s,%s)" (show_pattern e1) (show_pattern e2)
  | PNil -> "[]"
  | PCons (e1, e2) -> Printf.sprintf "%s::%s" (show_pattern e1) (show_pattern e2) 
  | PNone -> "None"
  | PSome m -> Printf.sprintf "Some %s" (show_pattern m)
  | PString s -> Printf.sprintf "\"%s\"" s
  | POp (op, m1, m2) -> Printf.sprintf "%s %s %s" (show_pattern m1) op (show_pattern m2)
  | PMinus m -> Printf.sprintf "-%s" (show_pattern m)
  | PIdentity -> "Id"
  | PConst p -> "Const "^(show_pattern p)
  | PCompose (f, g) ->
     "("^show_pattern f^") "^". ("^show_pattern g^")" 
  | PIsnum p -> "Num "^(show_pattern p)
  | PWhen(expr, pattern) -> Printf.sprintf "%s when %s" (show_pattern pattern) (show expr)
 
and show_function def = 
  "function\n" 
    ^ (String.concat "| " (List.map 
        (fun (m, e) -> 
           Printf.sprintf "%s -> %s\n" (show_pattern m) (show e)) def))

and show = function
  | EVariable v -> v
  | ENum n -> Int32.to_string n
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
    show_list (get_list (ECons(e1, e2)))
   
  | EApplication (EApplication (EVariable op, e1), e2) 
      when List.mem op ["+"; "*"; "/"; "^"; "=="; ">="; "<="; ">"; "<"] -> 
      Printf.sprintf "(%s %s %s)" (show e1) op (show e2)
  | EApplication (EVariable f, EVariable x) -> Printf.sprintf "%s %s" f x
  | EApplication (EVariable s, ENum x) -> Printf.sprintf "%s %s" s (Int32.to_string x)
  | EApplication(EVariable f, r) -> f ^ " (" ^ show r ^ ")"
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
  | EDeclare (var, Some expr) ->
      "declare " ^ var ^ " in " ^ show expr
  | EDeclare (var, None) -> "declare "^var

and get_list = function
  | ECons(x, xs) -> (show x)::get_list xs
  | _ -> []

let show_env env = 
  let rec show_mod = function
    | [] -> ()
    | (f, _, _) :: xs -> print_endline (" --- " ^ f); show_mod xs
  in
  print_endline ("Name : " ^ env.this);
  print_endline ("Loaded stuff : ");
  List.map (function (m, c) -> print_endline (" - Module : " ^ m); show_mod c) env.modules

