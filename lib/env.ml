module S = Symbol
module T = Types

type enventry = VarEntry of {ty:ty}
              | FunEntry of {formals:ty list; result: ty}

let base_tenv = Symbol.from [(S.symbol "int", T.INT); (S.symbol "string", T.STRING)]
let base_venv = Symbol.from []