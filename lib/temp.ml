type temp = int
let temps = ref 100
let newtemp() = let t = !temps in temps := t+1; t

module Table = Map.Make (struct
    type t = temp
    let compare = Stdlib.compare
  end)

type 'a table = 'a Table.t

let init l  = Table.of_seq (List.to_seq l)

let makestring t = "t" ^ string_of_int t

type label = Symbol.symbol

let labs = ref 0
let newlabel() =  let i = !labs in labs := i+1;
                  Symbol.symbol ("L" ^ string_of_int i)
let namedlabel = Symbol.symbol