open Syntax
open Modules
open Error 
open Helper

type simple_type = 
  | Variable of variable_of_type
  | Term of string * simple_type array
and variable_of_type = 
  { mutable level: int; mutable value: value_of_variable }
and value_of_variable = 
    | Unknow
    | Know of simple_type

type type_schema = 
 { parameter : variable_of_type list; corps : simple_type }

let type_unit = Term("unit", [||])
let type_int = Term("int", [||])
let type_bool = Term("bool", [||])
let type_string = Term("string", [||])
let type_product t1 t2 = Term("*", [|t1; t2|])
let type_list t = Term("list", [|t|])
let type_option t = Term("option", [|t|])
let type_arrow t1 t2 = Term("->", [|t1; t2|])

let level_of_liaison = ref 0;;
let new_unknow () = Variable {level = !level_of_liaison; value = Unknow }

let trivial_schema ty = {parameter = []; corps = ty}

let rec value_of = function
  | Variable ({value = Know ty1} as var) ->
    let value_of_ty1= value_of ty1 in
    var.value <- Know value_of_ty1;
    value_of_ty1
  | ty -> ty

let name_of_variables = ref ([] : (variable_of_type * string) list)

let print_var var = 
  "`" ^
  (try List.assq var !name_of_variables
   with Not_found ->
    let variables = List.fold_left 
      (fun set (_, x) -> StringSet.add x set) 
      StringSet.empty !name_of_variables 
    in
    let name = new_variable variables "a" in
    name_of_variables := (var, name) :: !name_of_variables;
    name)
;;
let rec print ty = match value_of ty with
  | Variable var -> print_var var
  | Term(constructor, arguments) ->
    match Array.length arguments with
      | 0 -> constructor
      | 1 -> Printf.sprintf "%s %s" (print arguments.(0)) constructor
      | 2 ->
	Printf.sprintf "(%s %s %s)" (print arguments.(0)) constructor (print arguments.(1))
;;
let print_type ty = 
  name_of_variables := []; print ty
;;


let loop_test var ty =
  let rec test t = match value_of t with
      | Variable var' ->
	if var == var' then raise(Loop (print_type (Variable var), print_type ty))
      | Term(_, arguments) ->
	Array.iter test arguments
in test ty;;


let rec rectify_levels level_max ty = match value_of ty with
  | Variable var ->
    if var.level > level_max then var.level <- level_max
  | Term(_, arguments) ->
    Array.iter (rectify_levels level_max) arguments;;


let rec unify ty1 ty2 = 
 
  print_string "unify "; print_string (print_type ty1); 
  print_string " and "; print_endline (print_type ty2);

  let v1 = value_of ty1
  and v2 = value_of ty2 in
  if v1 == v2 then () else
    match (v1, v2) with
      | Variable var, ty ->
	loop_test var ty;
	rectify_levels var.level ty;
	var.value <- Know ty
      | ty, Variable var ->
	loop_test var ty;
	rectify_levels var.level ty;
	var.value <- Know ty
      | Term(constr1, arguments1), Term(constr2, arguments2) ->
	if constr1 <> constr2 then 
	  raise (Conflit(print_type v1, print_type v2))
	else
	  for i = 0 to Array.length arguments1 - 1 do
	    unify arguments1.(i) arguments2.(i)
	  done
;;


let start_definition () = incr level_of_liaison
let end_definition () = decr level_of_liaison

let generalisation ty =
  let params = ref [] in
  let rec find_parameters ty = match value_of ty with
    | Variable var ->
      if var.level > !level_of_liaison && not (List.memq var !params)
      then params := var :: !params
    | Term(_, arguments) ->
      Array.iter find_parameters arguments 
  in
  find_parameters ty;
  {parameter = !params; corps = ty}
;;

let specialisation schema = match schema.parameter with
  | [] -> schema.corps
  | params ->
    let new_unknowns =
      List.map (fun var -> (var, new_unknow ())) params 
    in
    let rec copy ty = match value_of ty with
      | Variable var as ty ->
	(try List.assq var new_unknowns with Not_found -> ty)
      | Term(constr, arguments) ->
	Term(constr, Array.map copy arguments) 
    in
    copy schema.corps
;;

let rec type_pattern env = function
  | PVariable id ->
    let ty = new_unknow () in
    (ty, (id, trivial_schema ty) :: env)
  | PBoolean b ->
    (type_bool, env)
  | PNum n -> (type_int, env) 
  | PString _ -> (type_string, env)
  | PPair(m1, m2) ->
    let (ty1, env1) = type_pattern env m1 in
    let (ty2, env2) = type_pattern env1 m2 in
    (type_product ty1 ty2, env2)
  | PNone -> (type_option (new_unknow ()), env)
  | PSome p ->
    let ty, env' = type_pattern env p in
    (type_option ty, env')
  | PNil -> 
    (type_list (new_unknow ()), env)
  | PCons(m1, m2) ->
    let (ty1, env1) = type_pattern env m1 in
    let (ty2, env2) = type_pattern env1 m2 in
    unify (type_list ty1) ty2;
    (ty2, env2)
  | PAll -> (new_unknow (), env)
  | PMinus p ->
    let ty, env' = type_pattern env p in
    unify type_int ty;
    (ty, env')
  | POp (_, p1, p2) ->
    let (ty1, env1) = type_pattern env p1 in
    let (ty2, env2) = type_pattern env1 p2 in
    unify ty1 ty2;
    unify type_int ty1;
    (ty1, env2)
  | PApplication (f, x) ->
     let (type_argument, env1) = type_pattern env x in
     let (type_func, env2) = type_pattern env1 f in
     let type_result = new_unknow() in
     unify type_func (type_arrow type_argument type_result);
     (type_result, env2)


let rec type_expr env = function
  | ELet (def, None) ->
      let t, env' = type_def env def in
      (env', t)
  | EOpen (m, None) -> 
      let env' = open_module m
      in (env', type_unit)
  | expr -> (env, type_exp env expr)

and type_exp env = function
  | EVariable id ->
    begin 
      try specialisation (List.assoc id env)
      with Not_found -> raise (Error (id ^ " not found"))
    end
  | EFunction { def = list_of_cases } ->
    let type_argument = new_unknow ()
    and type_result = new_unknow () in
    let type_cas (pattern, expr) = 
      let type_pattern, extended_env = type_pattern env pattern in
      unify type_pattern type_argument;
      let type_expr = type_exp extended_env expr in
      unify type_expr type_result
    in
    List.iter type_cas list_of_cases;
    type_arrow type_argument type_result
  | EApplication(func, argument) ->
    let type_func = type_exp env func in
    let type_argument = type_exp env argument in
    let type_result = new_unknow () in
    unify type_func (type_arrow type_argument type_result);
    type_result
  | ELet(def, Some corps) -> 
    let _, env' = type_def env def in
    type_exp env' corps
  | EOpen (m, Some body) ->
      let env' = open_module m
      in type_exp (env' @ env) body
  | EBoolean _ -> type_bool
  | ENum _-> type_int
  | EString _ -> type_string
  | EPair (e1, e2) -> type_product (type_exp env e1) (type_exp env e2)
  | ENone -> type_option (new_unknow ())
  | ESome v -> type_option (type_exp env v)
  | ENil -> type_list (new_unknow ())
  | ECons(e1, e2) ->
    let t1 = type_exp env e1 in
    let t2 = type_exp env e2 in
    unify (type_list t1) t2;
    t2

and type_def env def = 
  start_definition ();
  let type_expr = match def.recursive with
    | false -> type_exp env def.expr
    | true ->
      let type_temporary = new_unknow () in
      let type_expr = 
	type_exp ((def.name, trivial_schema type_temporary) :: env) def.expr in
      unify type_expr type_temporary;
      type_expr 
  in
  end_definition ();
  (type_expr, (def.name, generalisation type_expr) :: env)

and do_type env = function
  | [] -> env
  | x :: xs -> 
      let (env', _) = type_expr env x
      in do_type env' xs

and open_module m = 
  let handle = open_in (file_from_module m) in
  let ast = parse Parser.file (Lexing.from_channel handle) 
  in close_in handle; do_type [] ast




