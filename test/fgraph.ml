open Tiger

let emitproc (out : out_channel) : Frame.frag -> unit = function
  | Frame.PROC {body; frame} ->
      let () = print_endline ("emit " ^ Symbol.name (Frame.name frame)) in
      let stms : Tree.stm list = Canon.linearize body in
      let stms' : Tree.stm list = Canon.traceSchedule (Canon.basicBlocks stms) in
      let instrs : Assem.instr list = List.concat (List.map (Codegen.codegen frame) stms') in
      let instrs2 = Frame.procEntryExit2 frame instrs in
      let fgraph, ordered_nodes = MakeGraph.instr2graph instrs2 in
      MakeGraph.show out fgraph ordered_nodes
  | Frame.STRING _ -> ()

let withOpenFile (fname : string) (f : out_channel -> unit) : unit =
  let out = open_out fname in
  try f out; close_out out with e -> close_out out; raise e

let () =
  for i = 1 to 49 do
    let filename = "testcases/test" ^ string_of_int i ^ ".tig" in
    let absyn = Parse.parse filename in
    let frags : Frame.frag list = FindEscape.findEscape absyn; Semant.transProg absyn in
    if !ErrorMsg.anyErrors then ()
    else
      withOpenFile
        ("test/fgraph/test" ^ string_of_int i ^ ".txt")
        (fun out -> List.iter (emitproc out) frags)
  done