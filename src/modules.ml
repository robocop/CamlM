open Error
open Syntax
open Helper

(* TODO : Fix cycling dependency issue *)

let include_path = ref ["."]
let module_ast = ref []

let module_present m e = 
  List.exists (function (m', _) -> m = m') e

let file_from_module module_name = 
  let files = 
    List.map (function p -> p ^ "/" ^ String.uncapitalize module_name ^ ".mml") !include_path
  in 
    try List.find Sys.file_exists files
    with Not_found -> raise (Error ("Could not find module " ^ module_name ^ " in search path"))

let load_module m = 
  if module_present m !module_ast then List.assoc m !module_ast
  else 
    let ast = bracket
                 (function _ -> open_in (file_from_module m))
                 (function h -> parse Parser.file (Lexing.from_channel h))
                 close_in
    in module_ast := (m, ast) :: !module_ast; ast

let open_module f m env = 
  if module_present m env.modules then env
  else 
    let ast = load_module m in
    let env' = f { env with this = m } ast
    in { env' with this = env.this } 

let value = function
  | (_, (v, _)) -> v

let op_prop = function
  | (_, (_, op)) -> op

let type_ = function
  | (_, (t, _)) -> t

let rec lookup_env' f name = function
  | [] -> []
  | (m, m_env) :: xs ->
    (match f name m_env with
       | None -> lookup_env' f name xs
       | Some x -> (m, x) :: lookup_env' f name xs)

let rec lookup_env f name env = match lookup_env' f name env.modules with
  | [] -> raise (Undef name)
  | [x] -> x
  | xs -> raise (MultiDef (name, List.map (function (m, _) -> m) xs))

let lookup_fun_env name env =
  let rec aux name = function
    | [] -> None
    | (y, value, op) :: _ when y = name -> 
        Some (value, op)
    | _ :: ys -> aux name ys
  in lookup_env aux name env

let lookup_type_env name env =
  let rec aux name = function
    | [] -> None
    | (y, t, r) :: _ when y = name -> 
        Some (t, r)
    | _ :: ys -> aux name ys
  in lookup_env aux name env

let rec add_mod m content = function
  | [] -> [(m, content)]
  | (m', contents) :: xs when m' = m ->
      (m', content @ contents) :: xs
  | x :: xs ->
      x :: add_mod m content xs

let multi_add_env content env = 
  { env with modules = add_mod env.this content env.modules }

let add_env content = multi_add_env [content]

