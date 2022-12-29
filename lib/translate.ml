type level  = Level of {parent : level;
                        frame: Frame.frame;
                        unique: unit ref}
            | Outermost

type access = level * Frame.access

let outermost = Outermost
let newLevel (parent, name, formals) =
  let frame = Frame.newFrame(name, true::formals) in
  Level{parent; frame; unique=ref()}

(*let formals lev = match lev with
  | Level{frame;_} -> List.map (fun fml -> (lev, fml)) (List.tl (Frame.formals frame))
  | Outermost -> []*)

let allocLocal(level, esc) =
  match level with
    | Level{frame; _} -> (level, Frame.allocLocal frame esc)
    | Outermost -> Errormsg.impossible "encounters Outermost"

type exp = unit