{ open Parser }

let char = ['\000'-'\033' '\035'-'\038' '\040'-'\127']
let num = ['0'-'9']+ ('.' ['0'-'9']+)? (['e' 'E'] '-'? ['0'-'9']+)?
let letter = ['a'-'z' 'A'-'Z']
let id = letter | ['0'-'9' '_']

rule token = parse
  | '#'         { HASH }
  | '_'         { UNDERSCORE }

  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { TIMES }
  | '/'         { DIV }
  | '$'         { DOLLAR }
  | "::"        { CONS }
  | "=="        { BEQ }
  | "!="        { BNEQ }
  | "<="        { BLEQ }
  | ">="        { BGEQ }
  | "<"         { BLT }
  | ">"         { BGT }
  | "not"       { BNOT }
  | "&&"        { BAND }
  | "||"        { BOR }
  | "true"      { BTRUE }
  | "false"     { BFALSE }
  | '('         { LPA }
  | ')'         { RPA }
  | ','         { COMMA }
  | '='         { EQ  }
  | ';'         { SEMI }
  | '['         { LSB }
  | ']'         { RSB }
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
  | '"' char* '"' 
    { let str = (Lexing.lexeme lexbuf)
      in STRING (String.sub str 1 (String.length str - 2)) }
  | num         { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | letter id*  { VAR (Lexing.lexeme lexbuf) }
  | '\n'        { Lexing.new_line lexbuf; token lexbuf }
  | _           { token lexbuf }
  | eof         { EOF }
