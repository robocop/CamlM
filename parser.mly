%{
    open Syntaxe
    let cons_op op a b = Application (Application(Variable op, a), b)
    let genLet r (nom, cas) dans = 
        Let({recursive=r; nom=nom; expr = cas}, dans)
%}

%token LPA RPA EOF END_EXPR
%token LET EQ IN COMMA ARROW PIPE FUNCTION REC SOME NONE UNDERSCORE
%token PLUS MINUS TIMES DIV CONS NIL
%token <int> NUM
%token <string> VAR

%nonassoc IN
%nonassoc LET
%nonassoc FUNCTION
%left PIPE
%left COMMA
%nonassoc ARROW
%right CONS
%left PLUS MINUS
%left TIMES DIV
%nonassoc SOME

%start eval
%type <Syntaxe.expression option> eval

%%

eval:
      EOF
        { None }
    | LET rec_flag let_bindings END_EXPR
        { Some (genLet $2 $3 None) }
    | expr END_EXPR
        { Some $1 }
    | error
        { raise (ParseError (Parsing.symbol_start_pos ())) }

expr:
      simple_expr     
        { $1 }
    | LET rec_flag let_bindings IN expr
        { genLet $2 $3 (Some $5) }
    | expr MINUS expr
        { cons_op "-" $1 $3 }
    | expr PLUS expr
        { cons_op "+" $1 $3 }
    | expr DIV expr
        { cons_op "/" $1 $3 }
    | expr TIMES expr
        { cons_op "*" $1 $3 }
    | FUNCTION patterns
        { Fonction $2 }

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
    VAR EQ expr             { ($1, $3) }

patterns:
      pattern                { [$1] }
    | patterns PIPE pattern  { $3 :: $1 }

pattern:
    case ARROW expr  { ($1, $3) }

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
