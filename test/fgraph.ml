let emitproc (out : out_channel) : Tiger.Frame.frag -> unit = function
  | Tiger.Frame.PROC {body; frame} ->
      let () = print_endline ("emit " ^ Tiger.Symbol.name (Tiger.Frame.name frame)) in
      let stms : Tiger.Tree.stm list = Tiger.Canon.linearize body in
      let stms' : Tiger.Tree.stm list = Tiger.Canon.traceSchedule (Tiger.Canon.basicBlocks stms) in
      let instrs : Tiger.Assem.instr list =
        List.concat (List.map (Tiger.Codegen.codegen frame) stms')
      in
      let instrs2 = Tiger.Frame.procEntryExit2 frame instrs in
      let fgraph, _ = Tiger.MakeGraph.instr2graph instrs2 in
      Tiger.MakeGraph.show out fgraph Tiger.Temp.makestring
  | Tiger.Frame.STRING _ -> ()

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
      ("test/fgraph/test" ^ string_of_int i ^ ".txt")
      (fun out -> List.iter (emitproc out) frags)
  done