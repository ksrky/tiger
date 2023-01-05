module T = Tree

let rec linearize(stm0: T.stm) : T.stm list =
  let
    (%) x y = match x, y with
    | (T.EXP(T.CONST _), x) -> x
    | (x, T.EXP(T.CONST _)) -> x
    | (x, y) -> T.SEQ(x, y) in

  let commute = function
    | (T.EXP(T.CONST _), _) -> true
    | (_, T.NAME _) -> true
    | (_, T.CONST _) -> true
    | _ -> false in

  let rec reorder = function
    | [] -> (T.EXP(T.CONST 0) , [])
    | (T.CALL(_) as e)::rest ->
	    let t = Temp.newtemp() in
      reorder(T.ESEQ (T.MOVE (T.TEMP t, e), T.TEMP t) :: rest)
    | a::rest ->
      let (stms, e) = do_exp a in
      let (stms', el) = reorder rest in
	    if commute(stms',e)
	      then (stms % stms',e::el)
	      else let t = Temp.newtemp() in
          (stms % T.MOVE(T.TEMP t, e) % stms', T.TEMP t :: el)

  and reorder_exp(el, build) =
    let (stms,el') = reorder el in
    (stms, build el')

  and reorder_stm(el, build) =
    let (stms, el') = reorder el in
    stms % build el'

  and do_stm = function
    | T.SEQ(a, b) -> do_stm a % do_stm b
    | T.JUMP(e, labs) -> reorder_stm([e], fun [e] -> T.JUMP(e, labs))
    | (T.CJUMP(p, a, b, t, f)) ->
      reorder_stm([a; b], fun [a; b] -> T.CJUMP(p, a, b, t, f))
    | T.MOVE(T.TEMP t,T.CALL(e, el)) -> 
      reorder_stm(e::el, fun (e::el) -> T.MOVE(T.TEMP t, T.CALL(e, el)))
    | T.MOVE(T.TEMP t,b) ->
      reorder_stm([b], fun [b] -> T.MOVE(T.TEMP t, b))
    | T.MOVE(T.MEM e, b) ->
      reorder_stm([e; b], fun [e; b] -> T.MOVE(T.MEM e, b))
    | T.MOVE(T.ESEQ(s, e), b) ->
      do_stm(T.SEQ(s, T.MOVE(e, b)))
    | T.EXP(T.CALL(e, el)) ->
      reorder_stm(e::el, fun (e::el) -> T.EXP(T.CALL(e, el)))
    | T.EXP e -> reorder_stm([e], fun [e] -> T.EXP e)
    | s -> reorder_stm([], fun [] -> s)

  and do_exp = function
    | T.BINOP(p, a, b) -> reorder_exp([a; b], fun [a; b] -> T.BINOP(p, a, b))
    | T.MEM(a) -> reorder_exp([a], fun [a] -> T.MEM(a))
    | T.ESEQ(s, e) ->
      let stms = do_stm s in
      let (stms', e) = do_exp e in
      (stms % stms', e)
    | T.CALL(e, el) -> reorder_exp(e::el, fun (e::el) -> T.CALL(e, el))
    | e -> reorder_exp([],fun [] -> e)

  and linear : T.stm * T.stm list -> T.stm list = function
    | (T.SEQ(a, b), l) -> linear(a, linear(b, l))
    | (s, l) -> s::l

in linear(do_stm stm0, [])

type block = T.stm list

let basicBlocks (stms: T.stm list) : (block list * Symbol.symbol) =
  let done' = Temp.newlabel() in
  let rec blocks = function
    | ((T.LABEL(_) as head) :: tail, blist) ->
      let rec next = function
        | (((T.JUMP _) as s)::rest, thisblock) -> endblock(rest, s::thisblock)
        | (((T.CJUMP _) as s)::rest, thisblock) -> endblock(rest, s::thisblock)
        | ((T.LABEL lab :: _ as stms), thisblock) ->
            next(T.JUMP(T.NAME lab,[lab]) :: stms, thisblock)
        | (s::rest, thisblock) -> next(rest, s::thisblock)
        | ([], thisblock) -> next([T.JUMP(T.NAME done', [done'])], thisblock)

      and endblock(stms, thisblock) = blocks(stms, List.rev thisblock :: blist)
      in next(tail, [head])
    | ([], blist) -> List.rev blist
    | (stms, blist) -> blocks(T.LABEL(Temp.newlabel())::stms, blist)
  in (blocks(stms, []), done')

let enterblock b t = match b, t with
  | ((T.LABEL s :: _) as b, table) -> Symbol.enter(table, s, b)
  | (_, table) -> table

let rec splitlast = function
  | [] -> ErrorMsg.impossible "Canon.splitlast got empty"
  | [x] -> ([], x)
  | h::t -> let (t',last) = splitlast t in (h::t', last)

let rec trace(table, ((T.LABEL lab :: _) as b),rest) = 
  let table = Symbol.enter(table, lab, []) in
  match splitlast b with
      | (most, T.JUMP(T.NAME lab, _)) ->
        (match Symbol.look(table, lab) with
          | Some(_::_ as b') -> most @ trace(table, b', rest)
          | _ -> b @ getnext(table,rest))
      | (most, T.CJUMP(opr, x, y, t, f)) ->
        (match (Symbol.look(table, t), Symbol.look(table, f)) with
          | (_, Some(_::_ as b')) -> b @ trace(table, b', rest)
          | (Some(_::_ as b'), _) -> 
              most @ [T.CJUMP(T.notRel opr, x, y, f, t)] @ trace(table, b', rest)
            | _ -> let f' = Temp.newlabel() in
              most
              @ [T.CJUMP(opr, x, y, t, f'); T.LABEL f'; T.JUMP(T.NAME f,[f])]
              @ getnext(table,rest))
      | (_, T.JUMP _) -> b @ getnext(table, rest)
      | _ -> ErrorMsg.impossible "Canon.trace got invalid arguments"
  and getnext = function
    | (table, ((T.LABEL lab::_) as b)::rest) -> 
      (match Symbol.look(table, lab) with
        | Some(_::_) -> trace(table,b,rest)
        | _ -> getnext(table,rest))
    | (_, []) -> []
    | _ ->  ErrorMsg.impossible "Canon.getnext got invalid arguments" 

let traceSchedule(blocks, done') =
  getnext(List.fold_right enterblock blocks Symbol.empty, blocks) @ [T.LABEL done']