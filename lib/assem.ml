type reg = string
type temp = Temp.temp
type label = Temp.label

type instr  = OPER of {assem: string;
			                dst: temp list;
			                src: temp list;
			                jump: label list option}
            | LABEL of {assem: string; lab: Temp.label}
            | MOVE of  {assem: string; dst: temp; src: temp}

let explode s = List.init (String.length s) (String.get s)
let implode l = String.of_seq (List.to_seq l)

let format (saytemp : temp -> reg) =
    let speak(assem, dst, src, jump) =
	    let saylab = Symbol.name in
		  let rec (f : char list -> char list) = function
        | ('`' :: 's' :: i :: rest) ->
		      (explode(saytemp(List.nth src (Char.code i - Char.code '0'))) @ f rest)
		    | ('`' :: 'd' :: i :: rest) -> 
		      (explode(saytemp(List.nth dst (Char.code i - Char.code '0'))) @ f rest)
		    | ('`' :: 'j' :: i :: rest) -> 
		      (explode(saylab(List.nth jump (Char.code i - Char.code '0'))) @ f rest)
        | ('`' :: '`':: rest) -> '`' :: f rest
        | ('`' :: _ :: _) -> ErrorMsg.impossible "bad Assem format"
        | c :: rest -> c :: f rest
        | [] -> []
      in implode(f(explode assem))
    in function
        | OPER{assem; dst; src; jump=None} -> speak(assem, dst, src, [])
        | OPER{assem; dst; src; jump=Some j} -> speak(assem, dst, src, j)
	      | LABEL{assem; _} -> assem
	      | MOVE{assem; dst; src} -> speak(assem, [dst], [src], [])