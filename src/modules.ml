open Error
open Syntax

let file_from_module module_name = 
  String.uncapitalize module_name ^ ".mml"

let value = function
  | (_, v, _) -> v

let op_prop = function
  | (_, _, op) -> op

let rec lookup_env' f = function
  | [] -> []
  | (m, m_env) :: xs -> 
      let rec lookup_env_content = function
        | [] -> None
        | (y, value, op) :: _ when y = f -> 
            Some (value, op)
        | _ :: ys -> lookup_env_content ys
      in (match lookup_env_content m_env with
            | None -> lookup_env' f xs
            | Some (value, op) -> (m, value, op) :: lookup_env' f xs)

let lookup_env f env = match lookup_env' f env.modules with
  | [] -> raise (Undef f)
  | [x] -> x
  | xs -> raise (MultiDef (f, List.map (function (m, _, _) -> m) xs))

let rec add_mod m content = function
  | [] -> [(m, content)]
  | (m', contents) :: xs when m' = m ->
      (m', content @ contents) :: xs
  | x :: xs ->
      x :: add_mod m content xs

let multi_add_env content env = 
  { env with modules = add_mod env.this content env.modules }

let add_env content = multi_add_env [content]

