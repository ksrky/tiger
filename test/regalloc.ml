let emitproc (out : out_channel) : Tiger.Frame.frag -> unit = function
  | Tiger.Frame.PROC {body; frame} ->
      let () = print_endline ("emit " ^ Tiger.Symbol.name (Tiger.Frame.name frame)) in
      let stms : Tiger.Tree.stm list = Tiger.Canon.linearize body in
      let stms' : Tiger.Tree.stm list = Tiger.Canon.traceSchedule (Tiger.Canon.basicBlocks stms) in
      let instrs : Tiger.Assem.instr list =
        List.concat (List.map (Tiger.Codegen.codegen frame) stms')
      in
      let instrs2 = Tiger.Frame.procEntryExit2 frame instrs in
      let instrs2', alloc = Tiger.RegAlloc.alloc instrs2 frame in
      let prolog, instrs3, epilog = Tiger.Frame.procEntryExit3 frame instrs2' in
      let format0 : Tiger.Assem.instr -> Tiger.Assem.reg =
        Tiger.Assem.format (fun t -> Tiger.Temp.Table.find t alloc)
      in
      output_string out prolog;
      List.iter (fun i -> output_string out (format0 i)) instrs3;
      output_string out epilog
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
      ("test/regalloc/test" ^ string_of_int i ^ ".txt")
      (fun out -> List.iter (emitproc out) frags)
  done