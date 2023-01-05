let withOpenFile (fname : string) (f : out_channel -> unit) : unit = 
  let out = open_out fname in
  try f out; close_out out
  with e -> close_out out; raise e

let printfrag out = function
  | Tiger.Frame.PROC{body; _} -> Tiger.PrintTree.printtree out body
  | Tiger.Frame.STRING(_, s) -> output_string out (s ^ "\n")

let () =
  for i = 1 to 49 do
    let filename = "testcases/test" ^ string_of_int i ^ ".tig" in
    let absyn = Tiger.Parse.parse filename in
    let flags : Tiger.Frame.frag list = (Tiger.FindEscape.findEscape absyn; Tiger.Semant.transProg absyn) in
    withOpenFile ("test/semant/test" ^ string_of_int i ^ ".txt") (fun out -> List.iter (printfrag out) flags)
  done