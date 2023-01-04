module T = Tree
module TP = Temp

type access = InFrame of int | InReg of TP.temp

type frame = {name: TP.label; formals: access list; locals: int ref}

type frag = PROC of {body: T.stm; frame: frame}
          | STRING of TP.label * string
type register = string

let wordSize = 4

(* registers of arguments *)
let a0 = TP.newtemp()
let a1 = TP.newtemp()
let a2 = TP.newtemp()
let a3 = TP.newtemp()

(* registers of temporaries *)
let t0 = TP.newtemp()
let t1 = TP.newtemp()
let t2 = TP.newtemp()
let t3 = TP.newtemp()
let t4 = TP.newtemp()
let t5 = TP.newtemp()
let t6 = TP.newtemp()
let t7 = TP.newtemp()

(* registers of saved temporaries *)
let s0 = TP.newtemp()
let s1 = TP.newtemp()
let s2 = TP.newtemp()
let s3 = TP.newtemp()
let s4 = TP.newtemp()
let s5 = TP.newtemp()
let s6 = TP.newtemp()
let s7 = TP.newtemp()

(* special registers *)
let fp = TP.newtemp()
let rv = TP.newtemp()
let sp = TP.newtemp()
let ra = TP.newtemp()

(* constant temporaries *)
let zero = TP.newtemp()

(* register list *)
let sepcialregs = [fp; rv; sp; ra]
let argregs = [a0; a1; a2; a3]
let calleesaves = [s0; s1; s2; s3; s4; s5; s6; s7]
let callersaves = [t0; t1; t2; t3; t4; t5; t6; t7]

let registers : register list = [ "$a0"; "$a1"; "$a2"; "$a3"
                                ; "$t0"; "$t1"; "$t2"; "$t3"
                                ; "$t4"; "$t5"; "$t6"; "$t7"
                                ; "$s0"; "$s1"; "$s2"; "$s3"
                                ; "$s4"; "$s5"; "$s6"; "$s7"
                                ; "$fp"; "$v0"; "$sp"; "$ra"]

let tempMap : register Temp.Table.t =
  Temp.Table.of_seq (List.to_seq 
    [(a0, "$a0"); (a1, "$a1"); (a2, "$a2"); (a3, "$a3");
     (t0, "$t0"); (t1, "$t1"); (t2, "$t2"); (t3, "$t3");
     (t4, "$t4"); (t5, "$t5"); (t6, "$t6"); (t7, "$t7");
     (s0, "$s0"); (s1, "$s1"); (s2, "$s2"); (s3, "$s3");
     (s4, "$s4"); (s5, "$s5"); (s6, "$s6"); (s7, "$s7");
     (fp, "$fp"); (rv, "$v0"); (sp, "$sp"); (ra, "$ra")])

let string (label, str) : string = Symbol.name label ^ ": .asciiz \"" ^ str ^ "\"\n" (*temp*)

let newFrame(name, escs) =
  let allocFormal fmls = function
    | true ->
      let n_fmls = List.length fmls in
      InFrame ((n_fmls+1) * wordSize) :: fmls
    | false -> InReg(TP.newtemp()) :: fmls in
  let formals = List.rev (List.fold_left allocFormal [] escs) in
  {name; formals; locals=ref 0}

let name {name; _} = name

let formals {formals; _} = formals

let allocLocal {locals; _} esc =
  if esc
    then (locals := !locals + 1; InFrame(- (!locals) * wordSize))
    else InReg(TP.newtemp())

let exp = function
  | InFrame(k) -> fun e -> T.MEM (T.BINOP (T.PLUS, e, T.CONST k))
  | InReg(t) -> fun _ -> T.TEMP t

let externalCall(name, args) = T.CALL (T.NAME (TP.namedlabel name), args)

let procEntryExit1(_, stm) = stm (*temp*)

let procEntryExit2 (_, body) =
  body @ [Assem.OPER{assem=""; src=[zero; ra; sp]@calleesaves; dst=[]; jump=None}]


(*let procEntryExit3({name; params; locals}, body) =
  {prolog="PROCEDURE " ^ Symbol.name name ^ "\n";
  body=body;
  epilog="END " ^ Symbol.name name ^ "\n"}*)