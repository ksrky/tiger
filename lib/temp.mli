type temp = int
val newtemp : unit -> temp
val makestring : temp -> string

module Table : sig
  type key = temp
  type 'a t
  val of_seq : (key * 'a) Seq.t -> 'a t
end

type label = Symbol.symbol
val newlabel : unit -> label
val namedlabel : string -> label