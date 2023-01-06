let emitproc (out : out_channel) : Frame.frag -> unit = function
  | Frame.PROC{body; frame} ->
    let () = print_endline ("emit " ^ Symbol.name(Frame.name frame)) in
    (*let () = PrintTree.printtree out body in*)
    let stms : Tree.stm list = Canon.linearize body in
    (*let () = List.iter (fun s -> PrintTree.printtree out s) stms in*)
    let stms' : Tree.stm list = Canon.traceSchedule(Canon.basicBlocks stms) in
    (*let () = List.iter (fun s -> PrintTree.printtree out s) stms' in*)
    let instrs : Assem.instr list = List.concat(List.map (Codegen.codegen frame) stms') in
    (* let instrs2 = Frame.procEntryExit2 (frame, instrs) in *)
    (* let (instrs2', alloc) = RegAlloc.alloc(instrs2, frame) stms' *)
    (* let {prolog; body; epilog} = Frame.procEntryExit3(frame, instrs2) *)
    let format0 : Assem.instr -> Assem.reg = Assem.format(Temp.makestring) in
    List.iter (fun i -> output_string out (format0 i)) instrs
  | Frame.STRING(lab, s) -> output_string out (Frame.string(lab, s))

let withOpenFile (fname : string) (f : out_channel -> unit) : unit = 
    let out = open_out fname in
    try f out; close_out out
    with e -> close_out out; raise e

let compile (filename : string) : unit =
  let absyn : Absyn.exp = Parse.parse filename in
  (*let () = PrintAbsyn.print(stdout, absyn) in*)
  let frags : Frame.frag list = (FindEscape.findEscape absyn; Semant.transProg absyn) in
  withOpenFile (filename ^ ".s") (fun out -> List.iter (emitproc out) frags)