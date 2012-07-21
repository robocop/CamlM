open OUnit

let usage = "usage: " ^ Sys.argv.(0) ^ " -exec [camlm]"
let camlm = ref ""
let speclist = [
  ("-exec", Arg.Set_string (camlm), "location of camlm executable")
]

let _ = 
  Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      usage; 

  print_endline (!camlm)
