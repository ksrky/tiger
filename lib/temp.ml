type temp = int
let temps = ref 100
let newtemp() = let t = !temps in temps := t+1; t

let makestring t = "t" ^ string_of_int t

type label = Symbol.symbol

let labs = ref 0
let newlabel() =  let i = !labs in labs := i+1;
                  Symbol.symbol ("L" ^ string_of_int i)
let namedlabel = Symbol.symbol