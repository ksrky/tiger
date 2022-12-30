module T = Tree
module TP = Temp

type access = InFrame of int | InReg of TP.temp

type frame = {name: TP.label; formals: access list; locals: int ref}

let wordSize = 4

let fp = TP.newtemp()

let newFrame(name, _) = {name; formals=[]; locals=ref 0} (*temp*)
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