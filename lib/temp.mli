type temp = int
val newtemp : unit -> temp
val makestring : temp -> string

module Table : sig
  type key = temp
  type 'a t
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val of_seq : (key * 'a) Seq.t -> 'a t
end

module Set : sig
  type elt = temp
  type t
  val empty : t
  val add : elt -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val elements : t -> elt list
  val of_list : elt list -> t
end

type label = Symbol.symbol
val newlabel : unit -> label
val namedlabel : string -> label