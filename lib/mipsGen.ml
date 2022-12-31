module A = Assem
module T = Tree

let codegen _ stm =
  let ilist = ref [] in
  let calldefs = [] in
  let emit instr = ilist := instr :: !ilist in
  let result gen = let t = Temp.newtemp() in gen t; t in
  let rec munchStm = function
    | T.SEQ(a, b) -> munchStm a; munchStm b
    | T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2) ->
      emit (A.OPER { assem="sw `s0, " ^ string_of_int i ^ "(`s1)\n"
                   ; src=[munchExp e2; munchExp e1]
                   ; dst=[]
                   ; jump=None})
    | T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2) ->
      emit (A.OPER { assem="sw `s0, " ^ string_of_int i ^ "(`s1)\n"
                   ; src=[munchExp e2; munchExp e1]
                   ; dst=[]
                   ; jump=None})
    | T.MOVE(T.MEM(T.CONST i), e1) ->
      emit (A.OPER { assem="sw `s0, " ^ string_of_int i ^ "(`r0)\n"
                   ; src=[munchExp e1]
                   ; dst=[]
                   ; jump=None})
    | T.MOVE(T.MEM e1, e2) ->
      emit (A.OPER { assem="sw `s0, 0(`s1)\n" (*or (`s1)*)
                   ; src=[munchExp e1; munchExp e2]
                   ; dst=[]
                   ; jump=None})
    | T.MOVE(T.TEMP i, e2) ->
      emit (A.OPER { assem="add `d0, `s0, r0\n"
                   ; src=[munchExp e2]
                   ; dst=[i]
                   ; jump=None})
    | T.LABEL lab ->
      emit (A.LABEL { assem=Symbol.name lab ^ ":\n"; lab=lab})
    | T.EXP(T.CALL(e, args)) ->
      emit (A.OPER { assem="CALL `s0\n"
                   ; src=munchExp e::munchArgs(0, args)
                   ; dst=calldefs
                   ; jump=None})
    | _ -> ErrorMsg.impossible ""

  and munchExp = function
    | T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)) ->
      result (fun r -> emit (A.OPER { assem="lw `d0, " ^ string_of_int i ^ "(`s0)"
                                    ; src=[munchExp e1]
                                    ; dst=[r]
                                    ; jump=None}))
    | T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)) ->
      result (fun r -> emit (A.OPER { assem="lw `d0, " ^ string_of_int i ^ "(`s0)"
                                    ; src=[munchExp e1]
                                    ; dst=[r]
                                    ; jump=None}))
    | T.MEM(T.CONST i) ->
      result (fun r -> emit (A.OPER { assem="lw `d0, " ^ string_of_int i ^ "(`s0)"
                                    ; src=[]
                                    ; dst=[r]
                                    ; jump=None}))
    | T.MEM(e1) ->
      result (fun r -> emit (A.OPER { assem="lw `d0, 0(`s0)" (*or (`s1)*)
                                    ; src=[munchExp e1]
                                    ; dst=[r]
                                    ; jump=None}))
    | T.BINOP(T.PLUS, e1, T.CONST i) ->
      result (fun r -> emit (A.OPER { assem="addi `d0, `s0, " ^ string_of_int i
                                    ; src=[munchExp e1]
                                    ; dst=[r]
                                    ; jump=None}))
    | T.BINOP(T.PLUS, T.CONST i, e1) ->
      result (fun r -> emit (A.OPER { assem="addi `d0, `s0, " ^ string_of_int i
                                    ; src=[munchExp e1]
                                    ; dst=[r]
                                    ; jump=None}))
    | T.BINOP(T.PLUS, e1, e2) ->
      result (fun r -> emit (A.OPER { assem="add `d0, `s0, `s1"
                                    ; src=[munchExp e1; munchExp e2]
                                    ; dst=[r]
                                    ; jump=None}))
    | T.CONST i ->
      result (fun r -> emit (A.OPER { assem="li `d0, " ^ string_of_int i
                                    ; src=[]
                                    ; dst=[r]
                                    ; jump=None}))
    | T.TEMP t -> t
    | _ -> ErrorMsg.impossible ""

  and munchArgs = function (*temp*)
    | (_, []) -> []
    | (i, arg::rest) ->
      munchExp arg :: munchArgs(i+1, rest)
in munchStm stm; List.rev(!ilist)