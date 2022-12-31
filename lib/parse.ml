let parse (filename : string) : Absyn.exp =
  let () = (ErrorMsg.reset(); ErrorMsg.fileName := filename) in
	let file = open_in filename in 
	let content = really_input_string file (in_channel_length file) in
  let linebuf = Lexing.from_string content in
  Parser.prog Lexer.token linebuf
