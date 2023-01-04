type allocation = Frame.register Temp.Table.t

type input = { interference: Liveness.igraph
             ; initial: allocation
             ; spillCost : Graph.node -> int
             ; registers: Frame.register list}


val initial : Temp.temp list ref
val coalescedNodes : Temp.temp list ref
val coloredNodes : Temp.temp list ref

val color : input -> allocation * Temp.temp list