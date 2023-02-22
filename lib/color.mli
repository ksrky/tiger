type allocation = Frame.register Temp.Table.t

val color :
     Liveness.igraph * allocation * (Graph.node -> int) * Frame.register list
  -> allocation * Temp.temp list

module TTS : sig
  type elt = Temp.temp * Temp.temp

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val remove : elt -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val equal : t -> t -> bool

  val iter : (elt -> unit) -> t -> unit

  val cardinal : t -> int

  val elements : t -> elt list

  val choose : t -> elt

  val of_list : elt list -> t
end
