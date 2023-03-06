val anyErrors : bool ref

val fileName : string ref

val lineNum : int ref

val linePos : int list ref

val sourceStream : in_channel ref

val error : Absyn.pos -> string -> unit

exception Error

val impossible : string -> 'a

val reset : unit -> unit