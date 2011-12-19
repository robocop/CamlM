{ open Parser }

let num = ['0'-'9']+ ('.' ['0'-'9']+)? (['e' 'E'] '-'? ['0'-'9']+)?
let str = ['a'-'z' 'A'-'Z']+
rule token = parse
  | '_'         { UNDERSCORE }
  | '+'         { ADD }
  | '-'         { REM }
  | '*'         { MUL }
  | '/'         { DIV }
  | '('         { LPA }
  | ')'         { RPA }
  | ','         { VIRGULE }
  | '='         { EQ  }
  | "::"        { CONS }
  | "[]"        { NIL }
  | '|'         { PIPE }
  | "let"       { LET }
  | "rec"       { REC }
  | "function"  { FUNCTION }
  | "in"        { IN  }
  | "->"        { FLECHE }
  | "None"      { NONE }
  | "Some"      { SOME }
  | "match"     { MATCH }
  | "with"      { WITH }
  | ";;"        { FUN_SEP }
  | num         { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | str         { VAR (Lexing.lexeme lexbuf) }
  | '\n'        { Lexing.new_line lexbuf; token lexbuf }
  | _           { token lexbuf }
  | eof         { EOF }
