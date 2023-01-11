type allocation = Frame.register Temp.Table.t

val color :
     Liveness.igraph * allocation * (Graph.node -> int) * Frame.register list
  -> allocation * Temp.temp list
