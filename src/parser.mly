%{
    open Syntax
    
    let snd (_, b) = b

    let cons_op op a b = EApplication (EApplication (EVariable op, a), b)

    let mkOpen m e = EOpen (m, e)

    let rec mkFun (cases, e) = match cases with
        | [] -> e
        | x :: xs -> mkFun (xs, EFunction {def = [x, e]; env = None})

    let mkLet r (n, pats, exp) in_clause = 
        ELet ( { recursive = r
            ; name = n
            ; expr = mkFun (pats, exp) }
            , in_clause )  

    let rec mkApp e = function
        | [] -> e
        | x :: xs -> mkApp (EApplication (e, x)) xs


    let rec stdList = function
      | [] -> ENil
      | x :: xs -> ECons (x, stdList xs)

    let rec mkMotifList = function
        | [] -> PNil
        | x :: xs -> PCons (x, mkMotifList xs)


    let fn e = EFunction {def = List.rev e; env = None}

    let mkPreMinus = function
      | ENum n -> ENum (-n)
      | expr -> EApplication (EVariable "-", expr) 

    let mkMotifPreMinus = function
      | PNum n -> PNum (-n)
      | motif -> PMinus motif 


%}

%token LPA RPA LSB RSB SEMI EOF END_EXPR HASH DOLLAR
%token FUNCTION MATCH WITH FUN OPEN LARROW
%token LET EQ IN COMMA RARROW PIPE REC SOME NONE UNDERSCORE
%token PLUS MINUS TIMES DIV CONS CONCAT
%token BEQ BNEQ BLEQ BGEQ BLT BGT BAND BOR BNOT BTRUE BFALSE WHEN
%token <int> NUM
%token <string> VAR STRING MODULE

%right DOLLAR
%right COMPOSE
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
%type <Syntax.expression Syntax.interpreter> eval

%start file
%type <Syntax.expression list> file

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
    | OPEN MODULE END_EXPR
        { mkOpen $2 None }
    | expr END_EXPR
        { $1 }

expr:
      simple_expr simple_expr_list %prec funapp
        { mkApp $1 (List.rev $2) }
    | LET rec_flag let_bindings IN expr
        { mkLet $2 $3 (Some $5) }
    | OPEN MODULE IN expr
        { mkOpen $2 (Some $4) }
    | a=expr; f=op; b=expr
        { f a b }
    | FUNCTION patterns
        { fn $2 }
    | FUN multi_pattern
        { mkFun $2 }
    | MATCH expr WITH patterns
        { EApplication (fn $4, $2) }

%inline op:
    PLUS   { cons_op "+" }
  | CONCAT { cons_op "++" }
  | MINUS  { fun x y -> cons_op "+" x (EApplication (EVariable "-", y)) }
  | TIMES  { cons_op "*" }
  | DIV    { cons_op "/" }
  | CONS   { fun x y -> ECons (x, y) }
  | DOLLAR { fun x y -> EApplication (x, y)}
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
      NUM                         { ENum $1 }
    | BTRUE                       { EBoolean true }
    | BFALSE                      { EBoolean false }
    | STRING                      { EString $1 }
    | VAR                         { EVariable $1 }
    | SOME expr                   { ESome $2 }
    | NONE                        { ENone }
    | LPA expr RPA                { $2 }
    | MINUS expr %prec MINUS      { mkPreMinus $2}
    | BNOT expr                   { EApplication (EVariable "not", $2) } 
    | LPA expr COMMA expr RPA     { EPair ($2, $4) }
    | LSB list_sugar RSB          { stdList $2 } 

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
    (*  case WHEN expr RARROW expr { (PWhen ($3, $1), $5) } *)
    | case RARROW expr           { ($1, $3) }

cases_or_empty:
      /* empty */  { [] }
    | cases        { $1 }
cases:
      case       { [$1] }
    | cases case { $2 :: $1 }

case:
      case CONS case             { PCons ($1, $3) }
    | SOME case                  { PSome $2 }
    | NONE                       { PNone }
    | UNDERSCORE                 { PAll }
    | NUM                        { PNum $1 }
    | BTRUE                      { PBoolean true }
    | BFALSE                     { PBoolean false }
    | STRING                     { PString $1 }
    | VAR                        { PVariable $1 }
    | LSB list_pattern_sugar RSB { mkMotifList $2 }
    | LPA case RPA               { $2 }
    | LPA case COMMA case RPA    { PPair ($2, $4) }
    | case PLUS case             { POp ("+", $1, $3) }
    | case TIMES case            { POp ("*", $1, $3) }
    | MINUS case                 { mkMotifPreMinus $2 }
    | case DIV case              { POp ("/", $1, $3) }
    | case case                  { PApplication ($1, $2) }


list_pattern_sugar:
      /* empty */                  { [] }
    | case                         { [$1] }
    | case SEMI list_pattern_sugar { $1 :: $3 }

rec_flag:
      /* empty */  { false }
    | REC          { true }
