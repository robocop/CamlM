%{
  open Syntaxe
  let cons_op op a b = Application (Application(Variable op, a), b)
%}

%token LPA RPA EOF
%token LET EQ IN VIRGULE FLECHE PIPE FUNCTION REC
%token ADD REM MUL DIV POW CONS NIL
%token <int> NUM
%token <string> VAR

%start eval
%type <Syntaxe.expression option> eval

%%

eval:
  | /* Empty */         { None }
  | expr EOF            { Some $1 }
  | error               { failwith "Syntax error" }

expr:
  | expr expr           { Application($1, $2) }
  | VAR                 { Variable $1 }
  | NUM                 { Nombre $1 }
  | NIL                 { Nil }
  | expr CONS expr      { Cons($1, $3)}
  | LPA expr RPA        { $2 }
  | expr ADD expr       { cons_op "+" $1 $3 }
  | expr REM expr       { cons_op "-" $1 $3 }
  | expr MUL expr       { cons_op "*" $1 $3 }
  | expr DIV expr       { cons_op "/" $1 $3 }
  | LPA expr VIRGULE expr RPA { Paire($2, $4) }
  | FUNCTION filtrage   { Fonction $2 }
  | LET VAR EQ expr IN expr
      { Let({recursive=false; nom=$2; expr = $4}, $6) }
  | LET REC VAR EQ expr IN expr
      { Let({recursive=true; nom=$3; expr = $5}, $7) }

motif:
  | NUM                 { Motif_nombre $1}
  | VAR                 { Motif_variable $1 }
  | LPA motif VIRGULE motif RPA { Motif_paire($2, $4) }
  | NIL                 { Motif_nil }
  | motif CONS motif    { Motif_cons($1, $3)}

filtrage: 
  | cas { [$1] }
  | cas filtrage_liste { $1::$2 }

filtrage_liste:
  | PIPE cas filtrage_liste { $2 :: $3 }
  | PIPE cas { [$2] }
 
cas:
  | motif FLECHE expr  { ($1, $3) }
