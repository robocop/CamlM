open OUnit

let suite = "Mini caml test suite" >:::
            [Test_parser.suite]

let _ = 
  run_test_tt_main suite
