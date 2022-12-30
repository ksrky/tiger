type access
type frame
type frag = Proc of {body: Tree.stm; frame: frame}
          | STRING of Temp.label * string

val wordSize : int

val fp : Temp.temp
val rv : Temp.temp

val newFrame : (Temp.label * bool list) -> frame
val name : frame -> Temp.label
val formals : frame -> access list
val allocLocal : frame -> bool -> access
val exp : access -> Tree.exp -> Tree.exp
val externalCall : (string * Tree.exp list) -> Tree.exp
val procEntryExit1 : frame * Tree.stm -> Tree.stm