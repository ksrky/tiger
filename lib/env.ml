module S = Symbol
module T = Types

type ty = Types.ty
type enventry = VarEntry of {ty:ty}
              | FunEntry of {formals:ty list; result: ty}

let base_tenv = Symbol.init [(S.symbol "int", T.INT); (S.symbol "string", T.STRING)]
let base_venv = Symbol.init
  (List.map
    (fun (name, fmls, res) -> (S.symbol name, FunEntry {formals = fmls; result = res}))
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