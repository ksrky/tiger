type temp = int
val newtemp : unit -> temp
val makestring : temp -> string

type 'a table
val init : (temp * 'a) list -> 'a table

type label = Symbol.symbol
val newlabel : unit -> label
val namedlabel : string -> label