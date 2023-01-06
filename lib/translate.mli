type level = Level of {parent: level; frame: Frame.frame; unique: unit ref}

type access = level * Frame.access

val outermost : level

val newLevel : level * Symbol.symbol * bool list -> level

val allocLocal : level * bool -> access

type exp = Ex of Tree.exp | Nx of Tree.stm | Cx of (Temp.label * Temp.label -> Tree.stm)

val simpleVar : access * level -> exp

val fieldVar : exp * int -> exp

val subscriptVar : exp * exp -> exp

val nilExp : exp

val intExp : int -> exp

val stringExp : string -> exp

val callExp : level * level * Symbol.symbol * exp list -> exp

val plusExp : exp -> exp -> exp

val minusExp : exp -> exp -> exp

val timesExp : exp -> exp -> exp

val divideExp : exp -> exp -> exp

val ltExp : exp -> exp -> exp

val gtExp : exp -> exp -> exp

val leExp : exp -> exp -> exp

val geExp : exp -> exp -> exp

val eqExp : exp -> exp -> exp

val neqExp : exp -> exp -> exp

val recordExp : exp list -> exp

val seqExp : exp list -> exp

val assignExp : exp * exp -> exp

val ifThenExp : exp -> exp -> exp

val ifThenElseExp : exp -> exp -> exp -> exp

val whileExp : exp * exp * Symbol.symbol -> exp

val breakExp : Symbol.symbol -> exp

val letExp : exp list * exp -> exp

val arrayExp : exp -> exp -> exp
(*val functionDec : exp*)

val procEntryExit : level * exp -> unit

val reset : unit -> unit

val getResult : unit -> Frame.frag list