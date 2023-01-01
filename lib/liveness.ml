type igraph = IGRAPH of { graph: Graph.graph
                        ; tnode: Temp.temp -> Graph.node
                        ; gtemp: Graph.node -> Temp.temp
                        ; moves: (Graph.node * Graph.node) list}

module LiveSet = Set.Make(struct
    type t = Temp.temp
    let compare = Stdlib.compare
  end)

type liveSet = unit Temp.Table.t * Temp.temp list
type _liveMap = liveSet Graph.Table.t 

let interferenceGraph (_fg: Flow.flowgraph) : igraph * (Flow.Graph.node -> Temp.temp list) =
  raise ErrorMsg.Error

let show _ = ()