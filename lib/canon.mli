val linearize : Tree.stm -> Tree.stm list
val basicBlocks : Tree.stm list -> (Tree.stm list list * Tree.label)
val traceSchedule : Tree.stm list list * Tree.label -> Tree.stm list