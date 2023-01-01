module Graph = Graph

type flowgraph = FGRAPH of { control: Graph.graph
                           ; def: Temp.temp list Graph.Table.t
                           ; use: Temp.temp list Graph.Table.t
                           ; ismove: bool Graph.Table.t}

