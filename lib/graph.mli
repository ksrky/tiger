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

module Set : sig
  type elt = node
  type t
  val empty : t
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val elements : t -> elt list
  val singleton : elt -> t
end

val nodename : node -> string