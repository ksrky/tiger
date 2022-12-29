type level  = Level of {parent : level;
                        frame: Frame.frame;
                        unique: unit ref}
            | Outermost

type access = {level: level; access: Frame.access}

let outermost = Outermost
let newLevel (parent, name, formals) =
  let frame = Frame.newFrame(name, true::formals) in
  Level{parent; frame; unique=ref()}

let allocLocal(level, esc) =
  match level with
    | Level{frame; _} -> {level; access=Frame.allocLocal frame esc}
    | Outermost -> Errormsg.impossible "encounters Outermost"

type exp = unit