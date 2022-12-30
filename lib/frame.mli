type access
type frame

val wordSize : int

val fp : int

val newFrame : (Temp.label * bool list) -> frame
val name : frame -> Temp.label
val formals : frame -> access list
val allocLocal : frame -> bool -> access
val exp : access -> Tree.exp -> Tree.exp
val externalCall : (string * Tree.exp list) -> Tree.exp