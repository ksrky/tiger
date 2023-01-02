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

module Table : sig
  type key = node
  type 'a t
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
end

val nodename : node -> string