val instr2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list

val show : out_channel * Flow.flowgraph * (Temp.temp -> string) -> unit