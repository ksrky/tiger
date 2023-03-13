open Tiger

let withOpenFile (fname : string) (f : out_channel -> unit) : unit =
  let out = open_out fname in
  try f out; close_out out with e -> close_out out; raise e

let compile filename =
  let src = "testcases/" ^ filename ^ ".tig" in
  let absyn = Parse.parse src in
  withOpenFile ("test/parse/" ^ filename ^ ".txt") (fun out -> PrintAbsyn.print (out, absyn))

let () =
  try
    let filename = Sys.argv.(1) in
    compile filename
  with _ ->
    for i = 1 to 49 do
      let filename = "test" ^ string_of_int i in
      compile filename
    done