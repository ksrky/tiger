type level  = Level of {parent : level;
                        frame: Frame.frame;
                        unique: unit ref}
            | Outermost

type access = (level * Frame.access)

val outermost : level
val newLevel : (level * Symbol.symbol * bool list) -> level
val allocLocal : (level * bool) -> access 

type exp = unit