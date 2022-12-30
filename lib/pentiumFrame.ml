module T = Tree
module TP = Temp

type access = InFrame of int | InReg of TP.temp

type frame = {name: TP.label; formals: access list; locals: int ref}

type frag = Proc of {body: T.stm; frame: frame}
          | STRING of TP.label * string

let wordSize = 4

let fp = TP.newtemp()
let rv = TP.newtemp()

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
let   procEntryExit1 (_, stm) = stm (*temp*)