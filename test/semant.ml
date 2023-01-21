open Tiger

let emitproc (out : out_channel) : Frame.frag -> unit = function
  | Frame.PROC {body; frame} ->
      let () = print_endline ("emit " ^ Symbol.name (Frame.name frame)) in
      PrintTree.printtree out body
  | Frame.STRING (lab, s) -> output_string out (Frame.string (lab, s))

let withOpenFile (fname : string) (f : out_channel -> unit) : unit =
  let out = open_out fname in
  try f out; close_out out with e -> close_out out; raise e

let () =
  for i = 1 to 49 do
    let filename = "testcases/test" ^ string_of_int i ^ ".tig" in
    let absyn = Parse.parse filename in
    let frags = FindEscape.findEscape absyn; Semant.transProg absyn in
    if !ErrorMsg.anyErrors then ()
    else
      withOpenFile
        ("test/semant/test" ^ string_of_int i ^ ".txt")
        (fun out -> List.iter (emitproc out) frags)
  done