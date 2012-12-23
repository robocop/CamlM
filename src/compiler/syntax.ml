(** CamlM representation and environment types. *)
open Graph 

(** CamlM AST structure.
   
    A CamlM source is parsed into an {!expression} by the CamlM parser
    ({!Parser}). Base expressions include : 
   
     - A construct similiar to OCaml's option type ([Some a], [None]).
     - Ordered pairs of elements (a 2-tuple).
     - Lists.
     - Axiom declaration ([declare axiom]).
     - ...
   
    While most of these constructs can be easily understood by the average OCaml
    user, the [declare] syntax ({{!expression}[EDeclare]}) deserves some
    explanation. See the CamlM wiki (linked below).
   
    @see
    <https://github.com/robocop/CamlM/wiki/%C3%89l%C3%A9ments-de-s%C3%A9mantique-du-langage-CamlM>
    (in french) for further details.
  *)
type expression = 
  | EVariable of string
  | EFunction of closure
  | EApplication of expression * expression
  | ELet of definition * expression option
  | EDeclare of string * expression option
  | EOpen of string * expression option
  | EBoolean of bool
  | ENum of int32
  | EPair of expression * expression
  | EUnit
  | ENil
  | ECons of expression * expression
  | ENone  
  | EString of string
  | ESome of expression

(** Type for holding an environment. This type is parametrized - it is used in
    both {!Eval} and {!Typing}. The structure stays the same, but the payload
    differs based on the use.
   
    In the {!Eval} module, a {!fun_env_content} is held by the {!env} structure.
    In the {!Typing} module, it is a {!Typing.type_env_content}.
   
    The fields are as follow : 
     - {{!env}[this]} : name of the "current module". In the REPL, it will be
       "_toplevel". When evaluating module "Foo", it will be "Foo".
     - {{!env}[modules]} : directed graph of all opened modules. If a module B
       is exposed to a module A, there is an edge in between A and B.
     - {{!env}[namespace]} : associates names to a list of modules they occur
       in. Depending on the module, the payload (the "value" associated to the
       name in the current context) will be different.
  *)
and 'a env = {
  this: string;
  modules: string graph;
  namespace: (string * ((string * 'a) list)) list
}

(** Environment payload for expression evaluation (see {!Eval}). Holds the value
    of a name (None if the name is an axiom) and a list of properties (see
    {!prop}) if the name is an operator.
  *)
and fun_env_content = (expression option) * ((prop list) option)

(** Properties of an operator : {{!prop}[Com]} if operator is commutative,
    {{!prop}[Assoc]} if operator is associative.
  
    This is useful to shorten redundant patterns (e.g. no need to test for 
    patterns [(-a) + b] and [b + (-a)] as [(+)] is commutative).
  *)
and prop = 
  | Com
  | Assoc

(** Pattern matching structure. Represents a pattern in any pattern matching
    construct ([function ...], [match ... with], etc).
   
    It is possible to match against specific axioms using the "@" symbol (parsed
    into a {{!pattern}[PAxiom]}), or to disassemble functions around an
    operator. Again, see wiki.
   
    @see
    <https://github.com/robocop/CamlM/wiki/%C3%89l%C3%A9ments-de-s%C3%A9mantique-du-langage-CamlM>
    (in french) for further details.
  *)
and pattern = 
  | PAll
  | PVariable of string
  | PAxiom of string
  | PBoolean of bool
  | PNum of int32
  | PPair of pattern * pattern
  | PNil
  | PCons of pattern * pattern
  | PNone
  | PSome of pattern 
  | PString of string
  | POp of string * pattern * pattern
  | PMinus of pattern
  | PCompose of pattern * pattern
  | PIdentity
  | PConst of pattern
  | PIsnum of pattern
  | PWhen of expression * pattern

(** A function environment ({!env}) paired with a function. Your everyday
    closure, really. *)
and closure = 
    { def : (pattern * expression) list; 
	    mutable env : (fun_env_content env) option }

(** Name definition. Name attached to a potentially recursive function or a
    value. *)
and definition = 
    {
      recursive:bool;
      name:string;
      expr:expression
    }


(** Types for the typing system *)

and simple_type = 
  | Variable of variable_of_type
  | Term of string * simple_type array
and variable_of_type = 
  { mutable level: int; mutable value: value_of_variable }
and value_of_variable = 
    | Unknow
    | Know of simple_type
type type_schema = 
 { parameter : variable_of_type list; corps : simple_type }
;;
let type_unit = Term("unit", [||])
let type_num = Term("num", [||])
let type_bool = Term("bool", [||])
let type_string = Term("string", [||])
let type_product t1 t2 = Term("*", [|t1; t2|])
let type_list t = Term("list", [|t|])
let type_option t = Term("option", [|t|])
let type_arrow t1 t2 = Term("->", [|t1; t2|])



(** REPL parser types. *)
type 'a interpreter = 
  | INothing                 (** Nothing was entererd. *)
  | ICommand of string       (** REPL command (e.g. "#quit;;"). *)
  | IValue of 'a             (** Expression to evaluate. *)



(** {!fun_env_content} helper. *)
let value (value, _) = value
(** {!fun_env_content} helper. *)
let op_prop (_, prop) = prop

