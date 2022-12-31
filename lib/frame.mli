type access
type frame
type frag = PROC of {body: Tree.stm; frame: frame}
          | STRING of Temp.label * string
type register

val fp : Temp.temp
val rv : Temp.temp

val wordSize : int
val tempMap : register Temp.table

val newFrame : (Temp.label * bool list) -> frame
val name : frame -> Temp.label
val formals : frame -> access list
val allocLocal : frame -> bool -> access
val exp : access -> Tree.exp -> Tree.exp
val externalCall : (string * Tree.exp list) -> Tree.exp
val procEntryExit1 : frame * Tree.stm -> Tree.stm
val procEntryExit2 : frame * Assem.instr list -> Assem.instr list