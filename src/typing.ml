open Syntax
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

let type_int = Term("int", [||])
let type_bool = Term("bool", [||])
let type_product t1 t2 = Term("*", [|t1; t2|])
let type_list t = Term("list", [|t|])
let type_arrow t1 t2 = Term("->", [|t1; t2|])

let level_of_liaison = ref 0;;
let new_unknow () = Variable {level = !level_of_liaison; value = Unknow }

let trivial_schema ty = {parameter = []; corps = ty}

let unify a b = ();;

let start_definition () = incr level_of_liaison
let end_definition () = decr level_of_liaison
let specialisation s = s.corps;;
let generalisation e = { parameter = []; corps = e}


let rec value_of = function
  | Variable ({value = Know ty1} as var) ->
    let value_of_ty1= value_of ty1 in
    var.value <- Know value_of_ty1;
    value_of_ty1
  | ty -> ty


let type_scope : (string * type_schema) list ref = ref []


let rec type_pattern env = function
  | PVariable id ->
    let ty = new_unknow () in
    (ty, (id, trivial_schema ty) :: env)
  | PBoolean b ->
    (type_bool, env)
  | PNum n -> (type_int, env) 
  | PPair(m1, m2) ->
    let (ty1, env1) = type_pattern env m1 in
    let (ty2, env2) = type_pattern env1 m2 in
    (type_product ty1 ty2, env2)
  | PNil -> 
    (type_list (new_unknow ()), env)
  | PCons(m1, m2) ->
    let (ty1, env1) = type_pattern env m1 in
    let (ty2, env2) = type_pattern env m2 in
    unify (type_list ty1) ty2;
    (ty2, env2)

let rec type_exp env = function
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
    type_exp (type_def env def) corps
  | EBoolean _ -> type_bool
  | ENum _-> type_int
  | EPair (e1, e2) -> type_product (type_exp env e1) (type_exp env e2)
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
  (def.name, generalisation type_expr) :: env



let name_of_variables = ref ([] : (variable_of_type * string) list)

let print_var var = 
  print_string "`";
  try print_string (List.assq var !name_of_variables) 
  with Not_found ->
    let variables = List.fold_left 
      (fun set (_, x) -> StringSet.add x set) 
      StringSet.empty !name_of_variables 
    in
    let name = new_variable variables "a" in
    name_of_variables := (var, name) :: !name_of_variables;
    print_string name
;;
let rec print ty = match value_of ty with
  | Variable var -> print_var var
  | Term(constructor, arguments) ->
    match Array.length arguments with
      | 0 -> print_string constructor
      | 1 -> print arguments.(0); print_string " "; print_string constructor;
      | 2 ->
	begin
	  print_string "("; print arguments.(0); 
	  print_string " "; print_string constructor; 
	  print_string " "; print arguments.(1); print_string ")";
	end
;;
let print_type ty = 
  name_of_variables := []; print ty
;;


