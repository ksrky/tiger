type igraph =
  | IGRAPH of
      { graph: Graph.graph
      ; tnode: Temp.temp -> Graph.node
      ; gtemp: Graph.node -> Temp.temp
      ; moves: (Graph.node * Graph.node) list }

val interferenceGraph : Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)

val show : out_channel -> igraph -> unit