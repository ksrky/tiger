type allocation = Frame.register Temp.Table.t

type input = { interference: Liveness.igraph
             ; initial: allocation
             ; spillCost : Graph.node -> int
             ; registers: Frame.register list}

val color : input -> allocation * Temp.temp list