let () =
  try let filename = Sys.argv.(1) in
    Tiger.Main.compile(filename)
  with Invalid_argument(_) -> print_endline "tiger: fatal error: no input files"