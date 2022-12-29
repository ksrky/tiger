module S = Symbol
module T = Types
module TL = Translate

type ty = Types.ty
type enventry = VarEntry of {access: Translate.access; ty:ty}
              | FunEntry of {level: Translate.level;
                            label: Temp.label;
                            formals:ty list; result: ty}

let base_tenv = Symbol.init [(S.symbol "int", T.INT); (S.symbol "string", T.STRING)]
let base_venv = Symbol.init
  (List.map
    (fun (name, formals, result) -> (S.symbol name,
      let label = Temp.namedlabel name in
      let level = TL.newLevel(TL.outermost, label, List.map (fun _ -> false) formals) in
      FunEntry {level; label; formals; result}))
    [("print", [T.STRING], T.UNIT)
    ;("flush", [], T.UNIT)
    ;("getchar", [], T.STRING)
    ;("ord", [T.STRING], T.INT)
    ;("chr", [T.INT], T.STRING)
    ;("size", [T.STRING], T.INT)
    ;("substring", [STRING; INT; INT], INT)
    ;("concat", [STRING; STRING], STRING)
    ;("not", [INT], INT)
    ;("exit", [INT], UNIT)
    ])