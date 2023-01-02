module T = Tree

type level  = Level of {parent : level;
                        frame: Frame.frame;
                        unique: unit ref}
            | Outermost

type access = level * Frame.access

type exp  = Ex of T.exp
          | Nx of T.stm
          | Cx of (Temp.label * Temp.label -> T.stm)
  
let outermost = Outermost

let newLevel (parent, name, formals) =
  let frame = Frame.newFrame(name, true::formals) in
  Level{parent; frame; unique=ref()}

(*let formals lev = match lev with
  | Level{frame;_} -> List.map (fun fml -> (lev, fml)) (List.tl (Frame.formals frame))
  | Outermost -> []*)

let allocLocal(level, esc) =
  match level with
    | Level{frame; _} -> (level, Frame.allocLocal frame esc)
    | Outermost -> ErrorMsg.impossible "encounters Outermost"

let fragments = ref []

let getResult() : Frame.frag list = !fragments


let rec mkseq = function
  | [] -> T.EXP(T.CONST 0)
  | [stm] -> stm
  | stm::stms -> T.SEQ(stm, mkseq stms)

let unEx = function
  | Ex exp -> exp
  | Nx stm -> T.ESEQ(stm, T.CONST 0)
  | Cx genstm ->
    let r = Temp.newtemp() in
    let t = Temp.newlabel() in
    let f = Temp.newlabel() in
      T.ESEQ (mkseq [
        T.MOVE(T.TEMP r, T.CONST 1);
        genstm(t, f);
        T.LABEL f;
        T.MOVE(T.TEMP r, T.CONST 0);
        T.LABEL t
      ], T.TEMP r)

let unNx = function
  | Ex exp -> T.EXP exp
  | Nx stm -> stm
  | Cx genstm ->
    let t = Temp.newlabel() in
    mkseq [genstm(t, t); T.LABEL t]

let unCx = function
  | Ex exp -> (fun (t, f) -> T.CJUMP(T.NE, exp, T.CONST 0, t, f))
  | Nx _ -> ErrorMsg.impossible "Translate.unCx recieved Nx"
  | Cx genstm -> genstm

let rec calcStaticLink = function
  | (Outermost, _) -> ErrorMsg.impossible ""
  | (_, Outermost) -> ErrorMsg.impossible ""
  | (Level cur_lev, Level use_lev) ->
    if cur_lev.unique = use_lev.unique
      then T.TEMP(Frame.fp)
      else match Frame.formals cur_lev.frame with
        | [] -> ErrorMsg.impossible ""
        | sl::_ -> Frame.exp sl (calcStaticLink(cur_lev.parent, Level use_lev))

let simpleVar ((def_lev, acs), use_lev) =
  Ex (Frame.exp acs (calcStaticLink(def_lev, use_lev)))

let fieldVar(rec_exp, idx) =
  Ex
  (T.MEM
    (T.BINOP
      ( T.PLUS
      , unEx rec_exp
      , T.CONST (idx * Frame.wordSize)
      )
    )
  )

let subscriptVar(arr_exp, sub_exp) =
  Ex
  (T.MEM
    (T.BINOP
      ( T.PLUS
      , unEx arr_exp
      , T.BINOP
        ( T.MUL
        , unEx sub_exp
        , T.CONST (Frame.wordSize)
        )
      )
    )
  )

let nilExp = Ex(T.CONST 0)

let intExp i = Ex(T.CONST i)

let stringExp s =
  let lab = Temp.newlabel() in
  fragments := Frame.STRING(lab, s) :: !fragments;
  Ex (T.NAME lab)

let callExp = function
  | (Outermost, _, _, _) -> ErrorMsg.impossible "Translate.callExp passed Outermost"
  | (_, Outermost, _, _) -> ErrorMsg.impossible "Translate.callExp passed Outermost"
  | (Level fun_lev, Level cal_lev, fun_lab, arg_exps) ->
    if fun_lev.parent = Outermost
      then Ex (Frame.externalCall (Symbol.name fun_lab, List.map unEx arg_exps))
      else
        let sl = calcStaticLink(Level fun_lev, Level cal_lev) in
        Ex(T.CALL(T.NAME fun_lab, sl::(List.map unEx arg_exps)))

let binOp oper left right =
  let left' = unEx left in
  let right' = unEx right in
  Ex(T.BINOP(oper, left', right'))

let plusExp = binOp T.PLUS
let minusExp = binOp T.MINUS
let timesExp = binOp T.MUL
let divideExp = binOp T.DIV

let relOp oper left right =
  let left' = unEx left in
  let right' = unEx right in
  Cx(fun (t, f) -> T.CJUMP(oper, left', right', t, f))

let ltExp = relOp T.LT
let gtExp = relOp T.GT
let leExp = relOp T.LE
let geExp = relOp T.GE
let eqExp = relOp T.EQ
let neqExp = relOp T.NE

let recordExp fields =
  let r = Temp.newtemp() in
  let init =
    T.MOVE(
      T.TEMP r,
      Frame.externalCall(
        "malloc", [T.CONST(List.length fields * Frame.wordSize)]
      )
    ) in
  let rec loop (fields, idx) =
    match fields with
      | [] -> []
      | e :: rest ->
        T.MOVE(
          T.MEM (T.BINOP
            ( T.PLUS
            , T.TEMP r
            , T.CONST(idx * Frame.wordSize)
            ))
          , unEx e)
        :: loop(rest, idx + 1)
  in Ex (T.ESEQ (mkseq (init::loop(fields, 0)), T.TEMP r))

let rec seqExp = function
  | [] -> Nx(T.EXP(T.CONST 0))
  | [exp] -> exp
  | exp::exps -> Ex(T.ESEQ(unNx exp, unEx (seqExp exps)))

let assignExp(var, init) =
  let var' = unEx var in
  let init' = unEx init in
  Nx (T.MOVE(var', init'))

let ifThenExp test then' =
  let genstm = unCx test in
  let then'' = unNx then' in
  let t = Temp.newlabel() in
  let f = Temp.newlabel() in
  Ex (T.ESEQ (mkseq [
    genstm(t, f);
    T.LABEL t;
    then'';
    T.LABEL f;
  ], T.CONST 0)) (*if-then exp yield no value*)

let ifThenElseExp test then' else' =
  let genstm = unCx test in
  let then'' = unEx then' in
  let else'' = unEx else' in
  let r = Temp.newtemp() in
  let t = Temp.newlabel() in
  let f = Temp.newlabel() in
  let j = Temp.newlabel() in
  Ex (T.ESEQ (mkseq [
    genstm(t, f);
    T.LABEL t;
    T.MOVE(T.TEMP r, then'');
    T.JUMP(T.NAME j, [j]);
    T.LABEL f;
    T.MOVE(T.TEMP r, else'');
    T.LABEL j
  ], T.TEMP r))

let whileExp(test, body, done_lab) =
  let genstm = unCx test in
  let body' = unNx body in
  let test_lab = Temp.newlabel() in
  let body_lab = Temp.newlabel() in
  Nx (mkseq [
    T.LABEL test_lab;
    genstm(body_lab, done_lab);
    T.LABEL body_lab;
    body';
    T.JUMP(T.NAME test_lab, [test_lab]);
    T.LABEL done_lab
  ])

let breakExp(done_lab) =
  Ex (T.ESEQ (T.JUMP (T.NAME done_lab, [done_lab]), T.CONST 0))

let letExp(decs, body) =
  Ex(T.ESEQ(mkseq (List.map unNx decs), unEx body))

let arrayExp size init =
  let r = Temp.newtemp() in
  let args = List.map unEx [size; init] in
  Ex (T.ESEQ
    (T.MOVE
      ( T.TEMP r
      , Frame.externalCall("initArrray", args)
      )
    , T.TEMP r
    )
  )

 (*let functionDec (name, Level lev, body) =
  let body_exp =
      Nx
        (T.MOVE
          ( T.TEMP Frame.rv
          , Frame.procEntryExit1 (lev.frame, unEx body)
          ))  
  in procEntryExit(Level lev, body_exp)*)

let procEntryExit ((Level{frame; _}, body) : (level * exp)) : unit =
  let body' = Frame.procEntryExit1(frame, T.MOVE(T.TEMP Frame.rv, unEx body)) in
  fragments := Frame.PROC{frame; body=body'} :: !fragments