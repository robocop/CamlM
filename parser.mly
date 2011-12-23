%{
    open Syntaxe
    let cons_op op a b = Application (Application (Variable op, a), b)

    let rec mkFun (cases, e) = match cases with
        | [] -> e
        | x :: xs -> mkFun (xs, Fonction [x, e])

    let mkLet r (name, pats, exp) in_clause = 
        Let ( { recursive = r
              ; nom = name
              ; expr = mkFun (pats, exp) }
            , in_clause )  

    let rec mkApp e = function
        | [] -> e
        | x :: xs -> mkApp (Application (e, x)) xs

    let fn e = Fonction (List.rev e)
%}

%token LPA RPA EOF END_EXPR HASH
%token FUNCTION MATCH WITH FUN
%token LET EQ IN COMMA ARROW PIPE REC SOME NONE UNDERSCORE
%token PLUS MINUS TIMES DIV CONS NIL
%token <int> NUM
%token <string> VAR

%nonassoc IN
%nonassoc LET
%nonassoc FUNCTION FUN WITH
%left PIPE
%left COMMA
%nonassoc ARROW
%right CONS 
%left PLUS MINUS
%left TIMES DIV
%nonassoc SOME 
%left funapp

%start eval
%type <Syntaxe.expression Syntaxe.interpreter> eval

%%

eval:
      EOF
        { INothing }
    | HASH VAR END_EXPR
        { ICommand $2 }
    | LET rec_flag let_bindings END_EXPR
        { IValue (mkLet $2 $3 None) }
    | expr END_EXPR
        { IValue $1 }

expr:
     simple_expr simple_expr_list %prec funapp
        { mkApp $1 (List.rev $2) }
    | LET rec_flag let_bindings IN expr
        { mkLet $2 $3 (Some $5) }
    | expr CONS expr
        { Cons ($1, $3) }
    | expr MINUS expr
        { cons_op "-" $1 $3 }
    | expr PLUS expr
        { cons_op "+" $1 $3 }
    | expr DIV expr
        { cons_op "/" $1 $3 }
    | expr TIMES expr
        { cons_op "*" $1 $3 }
    | FUNCTION patterns
        { fn $2 }
    | FUN multi_pattern
        { mkFun $2 }
    | MATCH expr WITH patterns
        { Application (fn $4, $2) }

simple_expr_list:
      /* empty */ { [] }
    |  simple_expr_list simple_expr { $2 :: $1 } 

simple_expr:
      NUM                   { Nombre $1 }
    | VAR                   { Variable $1 }
    | NIL                   { Nil }
    | SOME expr             { CSome $2 }
    | NONE                  { CNone }
    | LPA expr RPA          { $2 }
    | MINUS NUM             { Nombre (- $2) }
    | LPA expr COMMA expr   { Paire ($2, $4) }

let_bindings:
    VAR cases_or_empty EQ expr             { ($1, $2, $4) }

multi_pattern:
    cases ARROW expr { ($1, $3) }

patterns:
      pattern                { [$1] }
    | patterns PIPE pattern  { $3 :: $1 }

pattern:
    case ARROW expr  { ($1, $3) }

cases_or_empty:
      /* empty */  { [] }
    | cases        { $1 }
cases:
      case       { [$1] }
    | cases case { $2 :: $1 }

case:
      case CONS case          { Motif_cons ($1, $3) }
    | SOME case               { Motif_some $2 }
    | NONE                    { Motif_none }
    | UNDERSCORE              { Motif_all }
    | NUM                     { Motif_nombre $1 }
    | VAR                     { Motif_variable $1 }
    | NIL                     { Motif_nil }
    | LPA case RPA            { $2 }
    | LPA case COMMA case RPA { Motif_paire ($2, $4) }

rec_flag:
      /* empty */  { false }
    | REC          { true }
