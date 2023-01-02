type temp = int
val newtemp : unit -> temp
val makestring : temp -> string

module Table : sig
  type key = temp
  type 'a t
  val of_seq : (key * 'a) Seq.t -> 'a t
end

module Set : sig
  type elt = temp
  type t
  val empty : t
  val add : elt -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val elements : t -> elt list
  val of_seq : elt Seq.t -> t
end

type label = Symbol.symbol
val newlabel : unit -> label
val namedlabel : string -> label