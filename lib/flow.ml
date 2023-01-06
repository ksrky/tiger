module Graph = Graph

type flowgraph =
  | FGRAPH of
      { control: Graph.graph
      ; def: Temp.Set.t Graph.Table.t
      ; use: Temp.Set.t Graph.Table.t
      ; ismove: bool Graph.Table.t }
