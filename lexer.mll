{ open Parser }

let num = ['0'-'9']+ ('.' ['0'-'9']+)? (['e' 'E'] '-'? ['0'-'9']+)?
let str = ['a'-'z' 'A'-'Z']
let id = str | ['0'-'9' '_']
rule token = parse
  | '#'         { HASH }
  | '_'         { UNDERSCORE }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { TIMES }
  | '/'         { DIV }
  | '('         { LPA }
  | ')'         { RPA }
  | ','         { COMMA }
  | '='         { EQ  }
  | ';'         { SEMI }
  | '['         { LSB }
  | ']'         { RSB }
  | '$'         { DOLLAR }
  | "::"        { CONS }
  | '|'         { PIPE }
  | "open"      { OPEN }
  | "let"       { LET }
  | "rec"       { REC }
  | "function"  { FUNCTION }
  | "in"        { IN  }
  | "->"        { RARROW }
  | "<-"        { LARROW }
  | "None"      { NONE }
  | "Some"      { SOME }
  | "match"     { MATCH }
  | "with"      { WITH }
  | ";;"        { END_EXPR }
  | '\\'        { FUN }
  | num         { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | str id*     { VAR (Lexing.lexeme lexbuf) }
  | '\n'        { Lexing.new_line lexbuf; token lexbuf }
  | _           { token lexbuf }
  | eof         { EOF }
