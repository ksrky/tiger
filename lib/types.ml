type unique = unit ref

type ty = RECORD of (Symbol.symbol * ty) list * unique
        | NIL
        | INT
        | STRING
        | ARRAY of ty * unique
        | NAME of Symbol.symbol * ty option ref
        | UNIT

let rec type2str = function
  | RECORD(fields, _) -> "{" ^ String.concat ", " (List.map (fun (lab, ty) ->
      Symbol.name lab ^ ": " ^ type2str ty) fields) ^ "}"
  | NIL -> "nil"
  | INT -> "int"
  | STRING -> "string"
  | ARRAY(ty,_) -> "array of " ^ type2str ty
  | NAME(id,_) -> Symbol.name id
  | UNIT -> "unit"