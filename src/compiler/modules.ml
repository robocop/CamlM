(** Functions related to the module subsystem (loading and environment
    manipulation).
  
    Note that module opening in CamlM is analogous to importing modules in
    Haskell. Names cannot be resolved until the module is opened (this will be
    true for fully qualified names, if/when they are implemented).

    TODO : 
     - Fix cyclic module dependency issues.
     - Fix Windows compatibility.
*)

open Error
open Syntax
open Helper
open Graph

(** Constant name for the default, pre-opened module in all CamlM sources. *)
let prelude = "Prelude"

(** Global list containing the module search path.
    This reference is only ever modified during REPL initialization. *)
let include_path = ref []

(** Global association list of all modules in memory. Associates module name to
    the module's parsed (but not evaluated) source code
  *)
let module_ast = ref []

(** "exists" function for {!module_ast} *)
let module_present m e = 
  List.exists (function (m', _) -> m = m') e

(** Given a module name (e.g. Foo, Bar), retrieves the correspondant file name
    (e.g. /include/path/1/foo.mml, /include/path/2/bar.mml) from the include 
    path. If multiple files exist, takes the first match (in the order of the 
    {!include_path}). *)
let file_from_module module_name = 
  let files = 
    List.map (function p -> p ^ "/" ^ String.uncapitalize module_name ^ ".mml") !include_path
  in 
    try List.find Sys.file_exists files
    with Not_found -> raise (Error ("Could not find module " ^ module_name ^ " in search path"))

(** Load module m from its corresponding file into the {!module_ast}. The file
    is read, parsed, and stored in the association list as (m, parsed source
    file) iff it is not already in memory. 
    If it is already in memory, it is just returned.
   
    @param m Module name (not path). *)
let load_module m = 
  if module_present m !module_ast then List.assoc m !module_ast
  else 
    (* Open, parse, then close *)
    let ast = bracket
                 (function _ -> open_in (file_from_module m))
                 (function h -> parse Parser.file (Lexing.from_channel h))
                 close_in
    in module_ast := (m, ast) :: !module_ast; ast

(** Open the module given a function operating on an environment and a syntax
    tree.
   
    Opening goes as follows : 
     - If there is an edge in the graph in between the current module (A) and
       the module to be loaded (B) , then B is already loaded and exposed to A.
     - If the node B is present in the graph but there is no edge from A to B,
       expose B by adding an edge.
     - Otherwise, retrieve the module from the {!module_ast} and populate the
       environment using f.
   
     @param f function used to populate the environment with data from the AST.
     @param m module name.
     @param env current environment.
  *)
let open_module f m env = 
  if is_adjacent env.modules env.this m then env 
  else if node_present m env.modules then { env with modules = add_arc env.this m env.modules}
  else
    let ast = load_module m in
    (* Also expose Prelude and itself to the new module.
    * If it is not exposed to itself, it cannot resolve its own names. *)
    let modules = add_arc m m (add_arc m prelude (add_node m env.modules)) in
    (* Operate on the ast from the "point of view" of m. *)
    let env' = f { env with this = m; modules = modules } ast
    in { env' with this = env.this; modules = add_arc env.this m env'.modules} 

(** {!List.assoc} lifted from exceptions into the option type. *)
let rec lookup_env' name namespace = 
  try Some (List.assoc name namespace)
  with Not_found -> None

(** When the environment lookup returns multiple names (or none at all),
    pick the required one or throw an exception. *)
let disambiguate env name = function 
  | [] -> raise (Undef name)
  (* When the module is within scope, return the content, otherwise Undef *)
  | [m, content] when distance env.modules env.this m < 2 -> content
  | [_, _] -> raise (Undef name)
  | modules -> 
      (* In case of ambiguity, take the "closest" variable, i.e. the one in the
       * current module's scope. This allows for shadowing. *)
      try List.assoc env.this modules
      with Not_found -> raise (MultiDef (name, List.map (function (m, _) -> m) modules))

(** Lookup a name in the environment.
   
    @raise Undef if the name was not found in the environment or was not
    exposed to the current module.
    @raise MultiDef if there are multiple equally suitable definitions of the name
    in different modules. 
  *)
let rec lookup_env name env = match lookup_env' name env.namespace with
  | None -> raise (Undef name)
  | Some names ->
      (* Limit the search to modules in current module's scope. *)
      let names' = List.filter 
                     (fun (x, _) -> 
                        List.mem x (adjacent_nodes env.modules env.this)
                     ) names
      in disambiguate env name names'
            
(** Add a name & content to an environment's namespace. The namespace is
    structured such that names are the key of the association list. Each value
    associated to those keys is another association list containing pairs of
    module * content.
  *)
let rec add_name m (name, content) = function
  | [] -> 
      [(name, [m, content])]
  | (name', presence) :: xs when name' = name -> 
      (name, ((m, content) :: presence)) :: xs
  | x :: xs -> 
      x :: add_name m (name, content) xs

(** Add a whole module's worth of content.
   
    @param content list of pairs of names to add to the namespace
  *)
let add_mod m namespace content = 
  List.fold_right (add_name m) content namespace

(** {!add_mod} lifted to environments. *)
let multi_add_env content env = 
  { env with namespace = add_mod env.this env.namespace content}

(** {!multi_add_env} over a single element. *)  
let add_env content = multi_add_env [content]

(** Wrapper to initialize the {!include_path} reference. *)
let init path = include_path := path
