%{
    open Syntaxe
    
    let snd (_, b) = b

    let cons_op op a b = Application (Application (Variable op, a), b)

    let rec mkFun (cases, e) = match cases with
        | [] -> e
        | x :: xs -> mkFun (xs, Fonction ([x, e], None))

    let mkLet r (name, pats, exp) in_clause = 
        Let ( { recursive = r
              ; nom = name
              ; expr = mkFun (pats, exp) }
            , in_clause )  

    let rec mkApp e = function
        | [] -> e
        | x :: xs -> mkApp (Application (e, x)) xs


    let rec stdList = function
      | [] -> Nil
      | x :: xs -> Cons (x, stdList xs)

    let rec mkMotifList = function
        | [] -> Motif_nil
        | x :: xs -> Motif_cons (x, mkMotifList xs)


    let fn e = Fonction ((List.rev e), None)
%}

%token LPA RPA LSB RSB SEMI EOF END_EXPR HASH DOLLAR
%token FUNCTION MATCH WITH FUN OPEN LARROW
%token LET EQ IN COMMA RARROW PIPE REC SOME NONE UNDERSCORE
%token PLUS MINUS TIMES DIV CONS CONCAT
%token BEQ BNEQ BLEQ BGEQ BLT BGT BAND BOR BNOT BTRUE BFALSE WHEN
%token CONST ID
%token <int> NUM
%token <string> VAR STRING

%right DOLLAR
%nonassoc IN
%nonassoc LET OPEN
%nonassoc FUNCTION FUN WITH
%left PIPE
%left COMMA
%nonassoc RARROW LARROW
%nonassoc WHEN
%left BAND BOR
%left BLEQ BGEQ BLT BGT
%left BEQ BNEQ
%right CONS 
%left PLUS MINUS
%right CONCAT
%left TIMES DIV
%nonassoc SOME BNOT CONST ID
%left funapp

%start eval
%type <Syntaxe.expression Syntaxe.interpreter> eval

%start file
%type <Syntaxe.expression list> file

%%

eval:
      EOF
        { INothing }
    | HASH VAR END_EXPR
        { ICommand $2 }
    | toplevel
        { IValue $1 }

file:
      EOF
        { [] }
    | toplevel file
        { $1 :: $2 }

toplevel:
      LET rec_flag let_bindings END_EXPR
        { mkLet $2 $3 None }
    | expr END_EXPR
        { $1 }

expr:
      simple_expr simple_expr_list %prec funapp
        { mkApp $1 (List.rev $2) }
    | LET rec_flag let_bindings IN expr
        { mkLet $2 $3 (Some $5) }
    | a=expr; f=op; b=expr
        { f a b }
    | FUNCTION patterns
        { fn $2 }
    | FUN multi_pattern
        { mkFun $2 }
    | MATCH expr WITH patterns
        { Application (fn $4, $2) }

%inline op:
    PLUS   { cons_op "+" }
  | CONCAT { cons_op "++" }
  | MINUS  { cons_op "-" }
  | TIMES  { cons_op "*" }
  | DIV    { cons_op "/" }
  | CONS   { fun x y -> Cons (x, y) }
  | DOLLAR { fun x y -> Application (x, y)}
  | BEQ    { cons_op "==" }
  | BNEQ   { cons_op "!=" }
  | BLEQ   { cons_op "<=" }
  | BGEQ   { cons_op ">=" }
  | BLT    { cons_op "<" }
  | BGT    { cons_op ">" }
  | BAND   { cons_op "&&" }
  | BOR    { cons_op "||" }


simple_expr_list:
      /* empty */ { [] }
    |  simple_expr_list simple_expr { $2 :: $1 } 

simple_expr:
      NUM                     { Nombre $1 }
    | BTRUE                   { Booleen true }
    | BFALSE                  { Booleen false }
    | STRING                  { String $1 }
    | VAR                     { Variable $1 }
    | SOME expr               { CSome $2 }
    | NONE                    { CNone }
    | LPA expr RPA            { $2 }
    | MINUS NUM               { Nombre (- $2) }
    | BNOT expr               { Application (Variable "not", $2) } 
    | LPA expr COMMA expr RPA { Paire ($2, $4) }
    | LSB list_sugar RSB      { stdList $2 } 

list_sugar:
      /* empty */          {  [] }
    | expr                 { [$1] }
    | expr SEMI list_rest  { ($1 :: $3) }

list_rest:
      expr                { [$1] }
    | expr SEMI list_rest { $1 :: $3 }

list_comp:
    | VAR LARROW expr                   { [$1, $3] }
    | VAR LARROW expr SEMI list_comp    { ($1, $3) :: $5 }

let_bindings:
    VAR cases_or_empty EQ expr             { ($1, $2, $4) }

multi_pattern:
    cases RARROW expr { ($1, $3) }

patterns:
      pattern                { [$1] }
    | patterns PIPE pattern  { $3 :: $1 }

pattern:
      case WHEN expr RARROW expr { (Motif_when ($3, $1), $5) }
    | case RARROW expr           { ($1, $3) }

cases_or_empty:
      /* empty */  { [] }
    | cases        { $1 }
cases:
      case       { [$1] }
    | cases case { $2 :: $1 }

case:
      case CONS case { Motif_cons ($1, $3) }
    | SOME case { Motif_some $2 }
    | NONE { Motif_none }
    | UNDERSCORE { Motif_all }
    | NUM { Motif_nombre $1 }
    | BTRUE { Motif_booleen true }
    | BFALSE { Motif_booleen false }
    | STRING { Motif_string $1 }
    | VAR { Motif_variable $1 }
    | LSB list_pattern_sugar RSB { mkMotifList $2 }
    | LPA case RPA { $2 }
    | LPA case COMMA case RPA { Motif_paire ($2, $4) }
    | CONST case { FMotif_const $2  }
    | case PLUS case { FMotif_add ($1, $3) }
    | case TIMES case  { FMotif_mult ($1, $3) }
    | ID             { FMotif_Id }


list_pattern_sugar:
      /* empty */                  { [] }
    | case                         { [$1] }
    | case SEMI list_pattern_sugar { $1 :: $3 }

rec_flag:
      /* empty */  { false }
    | REC          { true }
