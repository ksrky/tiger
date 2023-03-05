module T = Tree
module TP = Temp

type access = InFrame of int | InReg of TP.temp

type frame = {name: TP.label; formals: access list; locals: int ref; instrs: T.stm list}

type frag = PROC of {body: T.stm; frame: frame} | STRING of TP.label * string

type register = string

let wordSize = 4

(* registers for arguments *)
let a0, a1, a2, a3 = (TP.newtemp (), TP.newtemp (), TP.newtemp (), TP.newtemp ())

(* registers for temporariy variables *)
let t0, t1, t2, t3, t4, t5, t6, t7 =
  ( TP.newtemp ()
  , TP.newtemp ()
  , TP.newtemp ()
  , TP.newtemp ()
  , TP.newtemp ()
  , TP.newtemp ()
  , TP.newtemp ()
  , TP.newtemp () )

(* registers for variables that need to be saved *)
let s0, s1, s2, s3, s4, s5, s6, s7 =
  ( TP.newtemp ()
  , TP.newtemp ()
  , TP.newtemp ()
  , TP.newtemp ()
  , TP.newtemp ()
  , TP.newtemp ()
  , TP.newtemp ()
  , TP.newtemp () )

(* special registers - frame pointer, return value, stack pointer, return address *)
let fp, rv, sp, ra = (TP.newtemp (), TP.newtemp (), TP.newtemp (), TP.newtemp ())

(* other registers - constant zero *)
let zero = TP.newtemp ()

(* register list *)
let sepcialregs = [fp; rv; sp; ra]

let argregs = [a0; a1; a2; a3]

let calleesaves = [s0; s1; s2; s3; s4; s5; s6; s7]

let callersaves = [t0; t1; t2; t3; t4; t5; t6; t7]

let registers : register list =
  [ "$t0"; "$t1"; "$t2"; "$t3"; "$t4"; "$t5"; "$t6"; "$t7"; "$s0"; "$s1"; "$s2"; "$s3"; "$s4"; "$s5"
  ; "$s6"; "$s7"; "$a0"; "$a1"; "$a2"; "$a3"; "$fp"; "$v0"; "$sp"; "$ra" ]

let tempMap : register TP.Table.t =
  TP.Table.of_seq
    (List.to_seq
       [ (a0, "$a0"); (a1, "$a1"); (a2, "$a2"); (a3, "$a3"); (t0, "$t0"); (t1, "$t1"); (t2, "$t2")
       ; (t3, "$t3"); (t4, "$t4"); (t5, "$t5"); (t6, "$t6"); (t7, "$t7"); (s0, "$s0"); (s1, "$s1")
       ; (s2, "$s2"); (s3, "$s3"); (s4, "$s4"); (s5, "$s5"); (s6, "$s6"); (s7, "$s7"); (fp, "$fp")
       ; (rv, "$v0"); (sp, "$sp"); (ra, "$ra") ] )

let string (lab, str) : string = Symbol.name lab ^ ":\t.asciiz \"" ^ str ^ "\"\n"

let exp = function
  | InFrame k -> fun e -> T.MEM (T.BINOP (T.PLUS, e, T.CONST k))
  | InReg t -> fun _ -> T.TEMP t

let newFrame (name, escs) =
  let allocFormal i = function
    | true -> InFrame ((i + 1) * wordSize)
    | false -> InReg (TP.newtemp ())
  in
  let formals = List.rev (List.mapi allocFormal escs) in
  let viewshift acc r = T.MOVE (exp acc (T.TEMP fp), T.TEMP r) in
  (* temp: number of parameters > number of argregs *)
  let instrs = List.mapi (fun i acc -> viewshift acc (List.nth argregs i)) formals in
  (* temp: out of bounds *)
  {name; formals; locals= ref 0; instrs}

let name {name; _} = name

let formals {formals; _} = formals

let allocLocal {locals; _} esc =
  if esc then (
    locals := !locals + 1;
    InFrame (- !locals * wordSize) )
  else InReg (TP.newtemp ())

let externalCall (name, args) = T.CALL (T.NAME (TP.namedlabel name), args)

let rec mkseq = function
  | [] -> T.EXP (T.CONST 0)
  | [stm] -> stm
  | stm :: stms -> T.SEQ (stm, mkseq stms)

let procEntryExit1 (frame : frame) (stm : Tree.stm) : Tree.stm =
  (* note: ra and calleesaves are saved in temporaries and they are restoed at the exit of a function.
      Those move instructions that are not spilled will be removed at register allocation. *)
  let accregs = List.map (fun r -> (allocLocal frame false, r)) (ra :: calleesaves) in
  let saves = List.map (fun (a, r) -> T.MOVE (exp a (T.TEMP fp), T.TEMP r)) accregs in
  let restores = List.map (fun (a, r) -> T.MOVE (T.TEMP r, exp a (T.TEMP fp))) (List.rev accregs) in
  mkseq (frame.instrs @ saves @ [stm] @ restores)

let procEntryExit2 (_ : frame) (body : Assem.instr list) : Assem.instr list =
  body @ [Assem.OPER {assem= ""; src= [zero; ra; sp] @ calleesaves; dst= []; jump= None}]

let procEntryExit3 ({name; locals; _} : frame) (body : Assem.instr list) :
    string * Assem.instr list * string =
  let offset = (!locals + List.length argregs) * wordSize in
  ( (* prolog *)
    String.concat "\n"
      [ Symbol.name name ^ ":"; "\tsw\t$fp, 0($sp)" (* save old fp *)
      ; "\tmove\t$fp, $sp" (* make sp new fp *)
      ; "\taddi\t$sp, $sp, -" ^ string_of_int offset (* make new sp *); "" ]
  , body
  , (* epilog *)
    String.concat "\n"
      [ "\tmove\t$sp, $fp" (* restore old sp *); "\tlw\t$fp, 0($sp)" (* restore old fp *)
      ; "\tjr\t$ra" (* jump to return address *); ""; "" ] )
