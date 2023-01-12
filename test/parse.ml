open Tiger

let withOpenFile (fname : string) (f : out_channel -> unit) : unit =
  let out = open_out fname in
  try f out; close_out out with e -> close_out out; raise e

let () =
  for i = 1 to 49 do
    let filename = "testcases/test" ^ string_of_int i ^ ".tig" in
    let absyn = Parse.parse filename in
    withOpenFile
      ("test/parse/test" ^ string_of_int i ^ ".txt")
      (fun out -> PrintAbsyn.print (out, absyn))
  done