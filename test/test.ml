open OUnit

let usage = "usage: " ^ Sys.argv.(0) ^ " -exec [camlm]"
let camlm = ref ""
let speclist = [
  ("-exec", Arg.Set_string (camlm), "location of camlm executable")
]

let tests = 
  [("Addition", "5+2", "7");
   ("Multiplication", "5*3", "15");
   ("Calcul", "5*(4+1)", "25");
   ("Evaluation d'une fonction", "let f x = x*2+1 in f 3", "7");
   ("Fonction recursive", "let rec fac = function 0 -> 1 | n -> n * fac (n-1) in fac 5", "120");
   ("Test liste vide", "let test = function [] -> true | _ -> false in test []", "true");
   ("Passage d'une fonction en argument", 
    "let f a x = a+x in let g x = f 1 x in g 1", 
    "2"
   );
   ("fold_left", 
    "let rec fold_left f x0 = function
         [] -> x0
       | x::xs -> fold_left f (f x0 x) xs
     in 
     let sum l = fold_left (\\x y -> x+y) 0 l in sum [1;2;3]", 
    "6"
   );
   ("Opérateur -", "let x = 3 in -x", "-3");
   ("Lambda calcul, renommage", 
    "declare x in let g y = x+y in \\x -> g (x+1)", "\\y -> (x + (y + 1))");
   ("Commentaires", "(* test *) 1", "1");
   ("Test de l'arobase dans un filtrage", 
    "declare exp in 
     let test1 = function @exp -> true | _ -> false in
     let test2 = function exp -> true | _ -> false in
     ((test1 (\\x -> x+1), test1 exp), (test2 (\\x -> x+1), test2 exp))",
    "((false, true), (true, true))")
  ]


let _ = 
  Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      usage; 

  let stdin, stdout = Unix.open_process (!camlm^" --minimal -I lib/") in

  let read_stdin stdin = input_line stdin in
  let send stdout cmd = output_string stdout (cmd^";;\n"); flush stdout in
  let eval f = send stdout f; read_stdin stdin in
  let genere_test () = 
    List.map (fun (name, f, r) -> 
      let t _ = assert_equal (eval f) r in
      name >:: t
    ) tests
  in
  let suite = "CamlM test suite" >::: genere_test() in

  run_test_tt_main suite
  
