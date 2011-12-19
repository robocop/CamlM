%{
  open Syntaxe
  let cons_op op a b = Application (Application(Variable op, a), b)
  let genLet (r, nom, cas) dans = 
    Let({recursive=r; nom=nom; expr = cas}, dans)
%}

%token LPA RPA EOF FUN_SEP
%token LET EQ IN VIRGULE FLECHE PIPE FUNCTION REC SOME NONE
%token ADD REM MUL DIV CONS NIL
%token <int> NUM
%token <string> VAR

%start eval
%type <Syntaxe.expression option> eval

%%

eval:
| EOF               { None }
| expr FUN_SEP      { Some $1 }
| error             { raise (ParseError (Parsing.symbol_start_pos ())) }

expr:
| term1 CONS expr   { Cons ($1, $3) }
| term1             { $1 }

term1:
| term1 ADD term2   { cons_op "+" $1 $3 }
| term1 REM term2   { cons_op "-" $1 $3 }
| term2             { $1 }

term2:
| term2 MUL term3   { cons_op "*" $1 $3 }
| term2 DIV term3   { cons_op "/" $1 $3 }
| term3             { $1 }

term3:
| LET let_binding              { genLet $2 None }
| LET let_binding IN expr      { genLet $2 (Some $4) }
| term4                        { $1 }

term4:
| term4 atom        { Application ($1, $2) }
| atom              { $1 }

atom:
| LPA expr VIRGULE expr RPA  { Paire($2, $4) }
| LPA expr RPA        { $2 }
| VAR                 { Variable $1 }
| NUM                 { Nombre $1 }
| NIL                 { Nil }

let_binding:
| REC VAR EQ let_expr   { (true, $2, $4) }
| VAR EQ let_expr       { (false, $1, $3) }

let_expr:
| expr                { $1 }
| FUNCTION filtrage   { Fonction $2 }

filtrage:
| cas       { [$1] }
| filtrage2 { $1 }

filtrage2:
| PIPE cas filtrage2 { $2 :: $3 }
| PIPE cas           { [$2] }

cas:
| motif FLECHE expr  { ($1, $3) }

motif:
| motif_atom CONS motif  { Motif_cons($1, $3) }
| motif_atom             { $1 }

motif_atom:
| LPA motif RPA               { $2 }
| LPA motif VIRGULE motif RPA { Motif_paire($2, $4) }
| NUM                         { Motif_nombre $1}
| VAR                         { Motif_variable $1 }
| NIL                         { Motif_nil }
| SOME motif                  { Motif_some $2 }
| NONE                        { Motif_none }
