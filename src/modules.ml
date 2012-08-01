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

(* Need to stop module multiload here *)
let open_module f m env = 
    let ast = load_module m in
    let env' = f { env with this = m } ast
    in { env' with this = env.this } 

let rec lookup_env' name namespace = 
  try Some (List.assoc name namespace)
  with Not_found -> None

let disambiguate this name = function 
  | [] -> failwith "Should never happen"
  | [_, content] -> content
  | modules -> 
      (* In case of ambiguity, take the "closest" variable, i.e. the one in the
       * current module's scope. This allows for shadowing. *)
      try List.assoc this modules
      with Not_found -> raise (MultiDef (name, List.map (function (m, _) -> m) modules))

let rec lookup_env name env = match lookup_env' name env.namespace with
  | None -> raise (Undef name)
  | Some x -> disambiguate env.this name x
            
let rec add_name m (name, content) = function
  | [] -> 
      [(name, [m, content])]
  | (name', presence) :: xs when name' = name -> 
      (name, ((m, content) :: presence)) :: xs
  | x :: xs -> 
      x :: add_name m (name, content) xs

let add_mod m namespace content = 
  List.fold_right (add_name m) content namespace

let multi_add_env content env = 
  { env with namespace = add_mod env.this env.namespace content}

let add_env content = multi_add_env [content]

let prelude = "Prelude"
