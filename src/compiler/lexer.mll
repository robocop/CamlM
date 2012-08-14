{ open Parser 
}

let char = ['\000'-'\033' '\035'-'\038' '\040'-'\127']
let num = ['0'-'9']+ ('.' ['0'-'9']+)? (['e' 'E'] '-'? ['0'-'9']+)?
let letter = ['a'-'z' 'A'-'Z']
let downcase = ['a'-'z']
let upcase = ['A'-'Z']
let id = letter | ['0'-'9' '_' '\'']

rule token = parse
  | '#'         { HASH }
  | '_'         { UNDERSCORE }
  | "++"        { CONCAT }
  | "mod"       { MOD    }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { TIMES }
  | '/'         { DIV }
  | '^'         { POW }
  | '$'         { DOLLAR }
  | '@'         { AT }
  | "::"        { CONS }
  | "=="        { BEQ }
  | "!="        { BNEQ }
  | "<="        { BLEQ }
  | ">="        { BGEQ }
  | "<"         { BLT }
  | ">"         { BGT }
  | "(*"        { comment 1 lexbuf }
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
  | "when"      { WHEN }
  | "let"       { LET }
  | "declare"   { DECLARE }
  | "deriving"     { DERIV }
  | "bool"      { TBOOL }
  | "num"       { TNUM }
  | "rec"       { REC }
  | "function"  { FUNCTION }
  | "in"        { IN  }
  | "Id"        { ID  }
  | "Const"     { CONST }
  | "->"        { RARROW }
(*  | "<-"        { LARROW } *)
  | "None"      { NONE }
  | "Some"      { SOME }
  | "match"     { MATCH }
  | "with"      { WITH }
  | ";;"        { END_EXPR }
  | '\\'        { FUN }
  | "Num"       { PNUM }
  | '"' char* '"' 
    { let str = (Lexing.lexeme lexbuf)
      in STRING (String.sub str 1 (String.length str - 2)) }
  | num         { NUM (Int32.of_string (Lexing.lexeme lexbuf)) }
  | downcase id*{ VAR (Lexing.lexeme lexbuf) }
  | upcase id*  { MODULE (Lexing.lexeme lexbuf) }
  | '\n'        { Lexing.new_line lexbuf; token lexbuf }
  | '.'         { POINT }
  | _           { token lexbuf }
  | eof         { EOF }

(* Supports nested comments. *)
and comment depth = parse
  | "(*" { comment (depth + 1) lexbuf }
  | "*)" {
    if depth = 1 then token lexbuf
    else comment (depth - 1) lexbuf
  }
  | _ { comment depth lexbuf }
