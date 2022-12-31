%{
open Absyn
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

%left AND OR
%nonassoc LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start <exp> prog

%{
let pos((startpos, _) : (Lexing.position * Lexing.position)) : pos = startpos.pos_bol
%}

%%

let prog :=
  | ~=exp; EOF;                           <>

let lvalue :=
  | ~=id;                                 { SimpleVar(id, pos($loc)) }
  | ~=lvalue; DOT; ~=id;                  { FieldVar(lvalue, id, pos($loc)) }
  | ~=lvalue; LBRACK; ~=exp; RBRACK;      { SubscriptVar(lvalue, exp, pos($loc)) }

let exp :=
  | ~=lvalue;                             { VarExp lvalue }
  | NIL;                                  { NilExp }
  | int=INT;                              { IntExp int } 
  | MINUS; right=exp; %prec UMINUS        { OpExp{left=IntExp 0; oper=MinusOp; right; pos=pos($loc)} }
  | str=STRING;                           { StringExp(str, pos($loc)) }
  | func=id; LPAREN; ~=args; RPAREN;      { CallExp{func; args; pos=pos($loc)} }
  | left=exp; PLUS; right=exp;            { OpExp{left; oper=PlusOp; right; pos=pos($loc)} }
  | left=exp; MINUS; right=exp;           { OpExp{left; oper=MinusOp; right; pos=pos($loc)} }
  | left=exp; TIMES; right=exp;           { OpExp{left; oper=TimesOp; right; pos=pos($loc)} }
  | left=exp; DIVIDE; right=exp;          { OpExp{left; oper=DivideOp; right; pos=pos($loc)} }
  | left=exp; EQ; right=exp;              { OpExp{left; oper=EqOp; right; pos=pos($loc)} }
  | left=exp; NEQ; right=exp;             { OpExp{left; oper=NeqOp; right; pos=pos($loc)} }
  | left=exp; LT; right=exp;              { OpExp{left; oper=LtOp; right; pos=pos($loc)} }
  | left=exp; LE; right=exp;              { OpExp{left; oper=LeOp; right; pos=pos($loc)} }
  | left=exp; GT; right=exp;              { OpExp{left; oper=GtOp; right; pos=pos($loc)} }
  | left=exp; GE; right=exp;              { OpExp{left; oper=GeOp; right; pos=pos($loc)} }
  | test=exp; AND; ~=exp;                 { IfExp{test; then'=exp; else'=Some(IntExp 0); pos=pos($loc)} }
  | test=exp; OR; ~=exp;                  { IfExp{test; then'=IntExp 1; else'=Some exp; pos=pos($loc)} }
  | typ=id; LBRACE; ~=fields; RBRACE;     { RecordExp{fields; typ; pos=pos($loc)} }
  | LPAREN; ~=exps; RPAREN;               { SeqExp exps }
  | var=lvalue; ASSIGN; ~=exp;            { AssignExp{var; exp; pos=pos($loc)} }
  | IF; test=exp; THEN; then_=exp;        { IfExp{test; then'=then_; else'=None; pos=pos($loc)} }
  | IF; test=exp; THEN; then_=exp; ELSE; else_=exp;
                                          { IfExp{test; then'=then_; else'=Some else_; pos=pos($loc)} }
  | WHILE; test=exp; DO; body=exp;        { WhileExp{test; body; pos=pos($loc)} }
  | FOR; var=id; ASSIGN; lo=exp; TO; hi=exp; DO; body=exp;
                                          { ForExp{var; escape=ref true; lo; hi; body; pos=pos($loc)} }
  | BREAK;                                { BreakExp(pos($loc)) }
  | LET; ~=decs; IN; ~=exps; END;         { LetExp{decs; body=SeqExp exps; pos=pos($loc)} }
  | typ=id; LBRACK; size=exp; RBRACK; OF; init=exp;
                                          { ArrayExp{typ; size; init; pos=pos($loc)} }

let args ==
  separated_list(COMMA, exp)

let fields ==
  separated_list(COMMA, field)

let field :=
  | ~=id; EQ; ~=exp;                      { (id, exp, pos($loc)) }

let exps ==
  separated_list(SEMICOLON, exppos)

let exppos :=
  ~=exp;                                  { (exp, pos($loc)) }

let decs ==
  | list(dec)

let dec :=
  | ~=nonempty_list(fundec);              <FunctionDec>
  | ~=vardec;                             <>
  | ~=nonempty_list(tydec);               <TypeDec>

let fundec :=
  | FUNCTION; name=id; LPAREN; params=tyfields; RPAREN; EQ; body=exp;
                                          { {name; params; result=None; body; pos=pos($loc)} }
  | FUNCTION; name=id; LPAREN; params=tyfields; RPAREN; COLON; typ=tyid; EQ; body=exp;
                                          { {name; params; result=Some(typ, pos($loc(typ))); body; pos=pos($loc)} }

let vardec :=
  | VAR; name=id; ASSIGN; init=exp;       { VarDec{name; escape=ref true; typ=None; init; pos=pos($loc)} }
  | VAR; name=id; COLON; typ=tyid; ASSIGN; init=exp;
                                          { VarDec{name; escape=ref true; typ=Some(typ, pos($loc(typ))); init; pos=pos($loc)} }

let tydec :=
  | TYPE; name=tyid; EQ; ~=ty;            { {name; ty; pos=pos($loc)} }

let ty :=
  | ~=tyid;                               { NameTy(tyid, pos($loc)) }
  | LBRACE; ~=tyfields; RBRACE;           <RecordTy>
  | ARRAY; OF; ~=tyid;                    { ArrayTy(tyid, pos($loc)) }

let tyfields ==
  separated_list(COMMA, tyfield)      

let tyfield :=
  | name=id; COLON; typ=tyid;             { {name; escape=ref true; typ; pos=pos($loc)} }

let id :=
  | ~=ID;                                 <Symbol.symbol>

let tyid :=
  | ~=ID;                                 <Symbol.symbol>