let printfrag out = function
  | Tiger.Frame.PROC{body; _} -> Tiger.PrintTree.printtree out body
  | Tiger.Frame.STRING(_, s) -> output_string out (s ^ "\n")

let () =
  for i = 1 to 49 do
    let filename = "../testcases/test" ^ string_of_int i ^ ".tig" in
    print_endline ("> " ^ filename);
    let absyn = Tiger.Parse.parse filename in
    let flags : Tiger.Frame.frag list = (Tiger.FindEscape.findEscape absyn; Tiger.Semant.transProg absyn) in
    List.iter (printfrag stdout) flags
  done