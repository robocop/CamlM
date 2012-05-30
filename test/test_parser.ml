open OUnit
open Lexer
open Syntax

let parse str = 
  Parser.eval Lexer.token (Lexing.from_string (str ^ ";;"))

let test_prio _ = 
  let ast = 
    EApplication (EApplication (EVariable "+", 
                                EApplication (EApplication (EVariable "*", 
                                                            ENum 1),
                                              ENum 2)),
                  ENum 3)
  in 
  assert_equal (IValue ast) (parse "1*2+3")

let suite = "Test parser" >:::
            ["Test operator priority" >:: test_prio]

