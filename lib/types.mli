type unique = unit ref

type ty =
  | RECORD of (Symbol.symbol * ty) list * unique
  | NIL
  | INT
  | STRING
  | ARRAY of ty * unique
  | NAME of Symbol.symbol * ty option ref
  | UNIT

val type2str : ty -> string
