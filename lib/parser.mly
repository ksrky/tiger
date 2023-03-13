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

%right OR
%right AND
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start <exp> prog

%{
let lp((sp, ep) : (Lexing.position * Lexing.position)) : pos
  = ((sp.pos_lnum, sp.pos_cnum - sp.pos_bol + 1), (ep.pos_lnum, ep.pos_cnum - sp.pos_bol + 1))
%}

%%

let prog :=
  | ~=exp; EOF;                           <>

let lvalue :=
  | ~=id;                                 { SimpleVar(id, lp($loc)) }
  | ~=lvalue; DOT; ~=id;                  { FieldVar(lvalue, id, lp($loc)) }
  | ~=id; LBRACK; ~=exp; RBRACK;          { SubscriptVar(SimpleVar(id, lp($loc(id))), exp, lp($loc)) } (* required *)
  | ~=lvalue; LBRACK; ~=exp; RBRACK;      { SubscriptVar(lvalue, exp, lp($loc)) }

let exp :=
  | ~=lvalue;                             { VarExp lvalue }
  | NIL;                                  { NilExp }
  | int=INT;                              { IntExp int } 
  | MINUS; right=exp; %prec UMINUS        { OpExp{left=IntExp 0; oper=MinusOp; right; pos=lp($loc)} }
  | str=STRING;                           { StringExp(str, lp($loc(str))) }
  | func=id; LPAREN; ~=args; RPAREN;      { CallExp{func; args; pos=lp($loc)} }
  | left=exp; PLUS; right=exp;            { OpExp{left; oper=PlusOp; right; pos=lp($loc)} }
  | left=exp; MINUS; right=exp;           { OpExp{left; oper=MinusOp; right; pos=lp($loc)} }
  | left=exp; TIMES; right=exp;           { OpExp{left; oper=TimesOp; right; pos=lp($loc)} }
  | left=exp; DIVIDE; right=exp;          { OpExp{left; oper=DivideOp; right; pos=lp($loc)} }
  | left=exp; EQ; right=exp;              { OpExp{left; oper=EqOp; right; pos=lp($loc)} }
  | left=exp; NEQ; right=exp;             { OpExp{left; oper=NeqOp; right; pos=lp($loc)} }
  | left=exp; LT; right=exp;              { OpExp{left; oper=LtOp; right; pos=lp($loc)} }
  | left=exp; LE; right=exp;              { OpExp{left; oper=LeOp; right; pos=lp($loc)} }
  | left=exp; GT; right=exp;              { OpExp{left; oper=GtOp; right; pos=lp($loc)} }
  | left=exp; GE; right=exp;              { OpExp{left; oper=GeOp; right; pos=lp($loc)} }
  | test=exp; AND; ~=exp;                 { IfExp{test; then'=exp; else'=Some(IntExp 0); pos=lp($loc)} }
  | test=exp; OR; ~=exp;                  { IfExp{test; then'=IntExp 1; else'=Some exp; pos=lp($loc)} }
  | typ=id; LBRACE; ~=fields; RBRACE;     { RecordExp{fields; typ; pos=lp($loc)} }
  | LPAREN; ~=exps; RPAREN;               { SeqExp exps }
  | var=lvalue; ASSIGN; ~=exp;            { AssignExp{var; exp; pos=lp($loc)} }
  | IF; test=exp; THEN; then_=exp;        { IfExp{test; then'=then_; else'=None; pos=lp($loc)} }
  | IF; test=exp; THEN; then_=exp; ELSE; else_=exp;
                                          { IfExp{test; then'=then_; else'=Some else_; pos=lp($loc)} }
  | WHILE; test=exp; DO; body=exp;        { WhileExp{test; body; pos=lp($loc)} }
  | FOR; var=id; ASSIGN; lo=exp; TO; hi=exp; DO; body=exp;
                                          { ForExp{var; escape=ref true; lo; hi; body; pos=lp($loc)} }
  | BREAK;                                { BreakExp(lp($loc)) }
  | LET; ~=decs; IN; ~=exps; END;         { LetExp{decs; body=SeqExp exps; pos=lp($loc)} }
  | typ=id; LBRACK; size=exp; RBRACK; OF; init=exp;
                                          { ArrayExp{typ; size; init; pos=lp($loc)} }

let args ==
  separated_list(COMMA, exp)

let fields ==
  separated_list(COMMA, field)

let field :=
  | ~=id; EQ; ~=exp;                      { (id, exp, lp($loc)) }

let exps ==
  separated_list(SEMICOLON, exppos)

let exppos :=
  ~=exp;                                  { (exp, lp($loc(exp))) }

let decs ==
  | list(dec)

let dec :=
  | ~=nonempty_list(fundec);              <FunctionDec>
  | ~=vardec;                             <>
  | ~=nonempty_list(tydec);               <TypeDec>

let fundec :=
  | pos=loc(FUNCTION); name=id; LPAREN; params=tyfields; RPAREN; EQ; body=exp;
                                          { {name; params; result=None; body; pos} }
  | pos=loc(FUNCTION); name=id; LPAREN; params=tyfields; RPAREN; COLON; typ=tyid; EQ; body=exp;
                                          { {name; params; result=Some(typ, lp($loc(typ))); body; pos} }

let vardec :=
  | VAR; name=id; ASSIGN; init=exp;       { VarDec{name; escape=ref true; typ=None; init; pos=lp($loc)} }
  | VAR; name=id; COLON; typ=tyid; ASSIGN; init=exp;
                                          { VarDec{name; escape=ref true; typ=Some(typ, lp($loc(typ))); init; pos=lp($loc)} }

let tydec :=
  | TYPE; name=tyid; EQ; ~=ty;            { {name; ty; pos=lp($loc)} }

let ty :=
  | ~=tyid;                               { NameTy(tyid, lp($loc)) }
  | LBRACE; ~=tyfields; RBRACE;           <RecordTy>
  | ARRAY; OF; ~=tyid;                    { ArrayTy(tyid, lp($loc)) }

let tyfields ==
  separated_list(COMMA, tyfield)      

let tyfield :=
  | name=id; COLON; typ=tyid;             { {name; escape=ref true; typ; pos=lp($loc)} }

let id :=
  | ~=ID;                                 <Symbol.symbol>

let tyid :=
  | ~=ID;                                 <Symbol.symbol>

let loc(t) ==
  | ~=t;                                  { ignore(t); lp($loc) }