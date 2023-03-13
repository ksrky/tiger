let emitproc (out : out_channel) : Frame.frag -> unit = function
  | Frame.PROC {body; frame} ->
      let () = print_endline ("emit " ^ Symbol.name (Frame.name frame)) in
      let stms : Tree.stm list = Canon.linearize body in
      let stms' : Tree.stm list = Canon.traceSchedule (Canon.basicBlocks stms) in
      let instrs : Assem.instr list = List.concat (List.map (Codegen.codegen frame) stms') in
      let instrs2 = Frame.procEntryExit2 frame instrs in
      let instrs2', alloc = RegAlloc.alloc instrs2 frame in
      let prolog, instrs3, epilog = Frame.procEntryExit3 frame instrs2' in
      let format0 : Assem.instr -> Assem.reg =
        Assem.format (fun t ->
            try Temp.Table.find t alloc
            with Not_found ->
              ErrorMsg.impossible "Register allocation doesn't cover all temporaries" )
      in
      output_string out prolog;
      List.iter (fun i -> output_string out (format0 i)) instrs3;
      output_string out epilog
  | Frame.STRING (lab, s) -> output_string out (Frame.string (lab, s))

let withOpenFile (fname : string) (f : out_channel -> unit) : unit =
  let out = open_out fname in
  try f out; close_out out with e -> close_out out; raise e

let compile (filename : string) : unit =
  let absyn : Absyn.exp = Parse.parse filename in
  let frags : Frame.frag list = FindEscape.findEscape absyn; Semant.transProg absyn in
  if !ErrorMsg.anyErrors then ()
  else withOpenFile (filename ^ ".s") (fun out -> List.iter (emitproc out) frags)
