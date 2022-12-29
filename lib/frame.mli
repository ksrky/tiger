type frame
type access
val newFrame : (Temp.label * bool list) -> frame
val name : frame -> Temp.label
val formals : frame -> access list
val allocLocal : frame -> bool -> access