type temp = int

val newtemp : unit -> temp

val makestring : temp -> string

module Table : sig
  type key = temp

  type 'a t

  val empty : 'a t

  val mem : key -> 'a t -> bool

  val add : key -> 'a -> 'a t -> 'a t

  val find : key -> 'a t -> 'a

  val of_seq : (key * 'a) Seq.t -> 'a t
end

module Set : sig
  type elt = temp

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val remove : elt -> t -> t

  val union : t -> t -> t

  val diff : t -> t -> t

  val equal : t -> t -> bool

  val iter : (elt -> unit) -> t -> unit

  val cardinal : t -> int

  val elements : t -> elt list

  val choose : t -> elt

  val of_list : elt list -> t
end

type label = Symbol.symbol

val newlabel : unit -> label

val namedlabel : string -> label