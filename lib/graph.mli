type graph
type node

val nodes : graph -> node list
val succ : node -> node list
val pred : node -> node list
val adj : node -> node list
val eq :  node * node -> bool

val newGraph : unit -> graph
val newNode : graph -> node
exception GraphEdge
val mk_edge : node * node -> unit
val rm_edge : node * node -> unit

type 'a table

val nodename : node -> string