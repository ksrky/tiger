let emitproc (out : out_channel) : Tiger.Frame.frag -> unit = function
  | Tiger.Frame.PROC {body; frame} ->
      let () = print_endline ("emit " ^ Tiger.Symbol.name (Tiger.Frame.name frame)) in
      let stms : Tiger.Tree.stm list = Tiger.Canon.linearize body in
      (*let () = List.iter (Tiger.PrintTree.printtree out) stms in*)
      let stms' : Tiger.Tree.stm list = Tiger.Canon.traceSchedule (Tiger.Canon.basicBlocks stms) in
      List.iter (Tiger.PrintTree.printtree out) stms'
  | Tiger.Frame.STRING (lab, s) -> output_string out (Tiger.Frame.string (lab, s))

let withOpenFile (fname : string) (f : out_channel -> unit) : unit =
  let out = open_out fname in
  try f out; close_out out with e -> close_out out; raise e

let () =
  for i = 1 to 49 do
    let filename = "testcases/test" ^ string_of_int i ^ ".tig" in
    let absyn = Tiger.Parse.parse filename in
    let frags : Tiger.Frame.frag list =
      Tiger.FindEscape.findEscape absyn; Tiger.Semant.transProg absyn
    in
    withOpenFile
      ("test/canon/test" ^ string_of_int i ^ ".txt")
      (fun out -> List.iter (emitproc out) frags)
  done