{
open Parser

type pos = Absyn.pos
exception Error of pos * string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let space = ['\t' '\n' '\r' ' ']

let decimal = digit+
let id = alpha (alpha | digit | '_')*
let string = '\"' [^ '"']* '\"'

rule token = parse
| space+        { token lexbuf }

(* comments *)
| "//"          { line_comment lexbuf }
| "/*"          { block_comment lexbuf }

(* reserved keywords *)
| "while"       { WHILE }
| "for"         { FOR }
| "to"          { TO }
| "break"       { BREAK }
| "let"         { LET }
| "in"          { IN }
| "end"         { END }
| "function"    { FUNCTION }
| "var"         { VAR }
| "type"        { TYPE }
| "array"       { ARRAY }
| "if"          { IF }
| "then"        { THEN }
| "else"        { ELSE }
| "do"          { DO }
| "of"          { OF }
| "nil"         { NIL }

(* reserved symbols *)
| ","           { COMMA }
| ":"           { COLON }
| ";"           { SEMICOLON }
| "("           { LPAREN }
| ")"           { RPAREN }
| "["           { LBRACK }
| "]"           { RBRACK }
| "{"           { LBRACE }
| "}"           { RBRACE }
| "."           { DOT }
| "+"           { PLUS }
| "-"           { MINUS }
| "*"           { TIMES }
| "/"           { DIVIDE }
| "="           { EQ }
| "<>"          { NEQ }
| "<"           { LT }
| "<="          { LE }
| ">"           { GT }
| ">="          { GE }
| "&"           { AND }
| "|"           { OR }
| ":="          { ASSIGN }

(* integer, string, and identifier *)
| decimal as i  { INT (int_of_string i) }
| string as s   { STRING (String.sub s 1 (String.length s - 2)) }
| id as s       { ID s }

| _             { ErrorMsg.error (Lexing.lexeme_start lexbuf) "illegal charcter" ; token lexbuf }

(* end of a file *)
| eof           { EOF }

and line_comment = parse
| ('\n' | eof)  { token lexbuf }
| _             { line_comment lexbuf }

and block_comment = parse
| "*/"          { token lexbuf }
| eof           { ErrorMsg.error (Lexing.lexeme_start lexbuf) "unterminated comment"; token lexbuf }
| _             { block_comment lexbuf }