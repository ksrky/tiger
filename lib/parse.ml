let parse (filename : string) : Absyn.exp =
  let () = (ErrorMsg.reset(); ErrorMsg.fileName := filename) in
	let file = open_in filename in 
	let content = really_input_string file (in_channel_length file) in
  let linebuf = Lexing.from_string content in
  try Parser.prog Lexer.token linebuf
  with
    | Lexer.Error(pos, msg) -> ErrorMsg.error pos msg; Absyn.NilExp
    | Parser.Error -> ErrorMsg.error (Lexing.lexeme_start linebuf) "parse error"; Absyn.NilExp
    | _ -> ErrorMsg.impossible "unknown error returned by lexer or parser"
