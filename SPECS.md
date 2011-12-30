MINI-CAML V1 SPECIFICATIONS
===========================

Grammaire
---------

```
mcaml = expr ';;'
    | 'let' 'rec'? let_binding ';;'
    | 'use' string ';;'

expr = simple_expr
    | prefix_op expr
    | expr infix_op expr
    | 'let' 'rec'? let_binding 'in' expr
    | ('function' | 'match' expr 'with') patterns
    | '\' case+ '->' expr

simple_expr = id
    | integer
    | decimal
    | character
    | string
    | boolean
    | option<expr>
    | list_sugar<expr>
    | list_comprehension
    | '(' ')'
    | '(' expr ')'
    | '(' expr (',' expr)+ ')'
    | expr ':' type

let_binding = id case* '=' expr

case = simple_case
    | fun_case
    | case '::' case

simple_case = '_'
    | integer
    | decimal
    | character
    | string
    | boolean
    | option<case>
    | list_sugar<case>
    | '(' ')'
    | '(' case ')'
    | '(' case (',' case)+ ')'
    | case ':' type

fun_case = 'Const' case
    | 'Id'
    | prefix_op case
    | case infix_op case

patterns = pattern 
    | patterns '|' pattern

pattern = case ('when' expr)? '->' expr

prefix_op = 'not' | '-'
infix_op = '+' | '-' | '*' | '/'
    | '==' | '!=' | '<=' | '>=' | '<' | '>'
    | '&&' | '||'
    | '::'
    | '++'
    | '$'

list_comprehension = '[' expr '|' (case '<-' expr)+ expr* ']' 

type = (type ' ')* base_type
    | polymorphic_type
    | type '->' type
    | type '*' type

polymorphic_type = ''' id

base_type = 'int'
    | 'float'
    | 'string'
    | 'char'
    | 'list'
    | 'option'

character = ''' printable_char '''
string = '"' printable_char* '"'
boolean = 'true' | 'false'
option<T> = 'None'
    | 'Some' T
list_sugar<T> = '[' ']'
    | '[' T (';' T)* ']'
```

Sémantique
----------

To do
-----

- Ajout d'un typeur
- Ajout des fonctions/constantes définie sans leur expression.
    Exemple : 
    let def pi in  (* pi a le type num automatiquement *)
    let def function exp in (* exp a le type num -> num automatiquement *)
    let f x = \x * exp (x*x) in
    
    let rec test = function
          Const (def "pi") ->  true
        | F "exp" . g -> true
        | f + g -> test f || test g
        | _ -> false

- Ajout d'un type num supportant les flottants, les entiers arbitrairement long
- Ajout des listes de la forme : [1..n]
- Ajout des n-uples