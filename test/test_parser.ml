open OUnit
open Lexer
open Syntaxe

let parse str = 
  Parser.eval Lexer.token (Lexing.from_string (str ^ ";;"))

let test_prio _ = 
  let ast = 
    Application (Application (Variable "+", 
                              Application (Application (Variable "*", 
                                                        Nombre 1),
                                           Nombre 2)),
                 Nombre 3)
  in assert_equal (IValue ast) (parse "1*2+3")

let suite = "Test parser" >:::
            ["Test operator priority" >:: test_prio]

