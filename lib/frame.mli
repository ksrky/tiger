type access

type frame

type frag = PROC of {body: Tree.stm; frame: frame} | STRING of Temp.label * string

type register = string

val fp : Temp.temp

val rv : Temp.temp

val ra : Temp.temp

val argregs : Temp.temp list

val callersaves : Temp.temp list

val wordSize : int

val registers : register list

val tempMap : register Temp.Table.t

val string : Symbol.symbol * string -> string

val newFrame : Temp.label * bool list -> frame

val name : frame -> Temp.label

val formals : frame -> access list

val allocLocal : frame -> bool -> access

val exp : access -> Tree.exp -> Tree.exp

val externalCall : string * Tree.exp list -> Tree.exp

val procEntryExit1 : frame -> Tree.stm -> Tree.stm

val procEntryExit2 : frame -> Assem.instr list -> Assem.instr list

val procEntryExit3 : frame -> Assem.instr list -> string * Assem.instr list * string