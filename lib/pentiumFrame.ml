type access = InFrame of int | InReg of Temp.temp

type frame = {name: Temp.label; formals: access list; locals: int ref}

let wordSize = 4

let newFrame(name, _) = {name; formals=[]; locals=ref 0} (*temp*)
let name {name; _} = name
let formals {formals; _} = formals
let allocLocal {locals; _} esc =
  if esc
    then (locals := !locals + 1; InFrame(- (!locals) * wordSize))
    else InReg(Temp.newtemp())