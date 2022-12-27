%{
open Syntax
let addtyp x = (x, Type.gentyp ())
%}

%token EOF
%token <string> ID
%token <int> INT
%token <string> STRING
%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK
%token LBRACE RBRACE DOT
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE
%token AND OR ASSIGN
%token ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF
%token BREAK NIL
%token FUNCTION VAR TYPE

prog:
| exp

lvalue:
| varid  
| lvalue '.' varid
| lvalue '[' exp ']'

decs:
| dec*

dec
| tydecs
| vardec
| fundecs