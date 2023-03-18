module A = Assem
module T = Tree

let codegen _ stm =
  let ilist = ref [] in
  let calldefs = [Frame.rv; Frame.ra] @ Frame.callersaves in
  let emit instr = ilist := instr :: !ilist in
  let result gen =
    let t = Temp.newtemp () in
    gen t; t
  in
  let rec munchStm = function
    | T.SEQ (a, b) -> munchStm a; munchStm b
    | T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)), e2) ->
        emit
          (A.OPER
             { assem= "\tsw\t`s0, " ^ string_of_int i ^ "(`s1)\n"
             ; src= [munchExp e2; munchExp e1]
             ; dst= []
             ; jump= None } )
    | T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)), e2) ->
        emit
          (A.OPER
             { assem= "\tsw\t`s0, " ^ string_of_int i ^ "(`s1)\n"
             ; src= [munchExp e2; munchExp e1]
             ; dst= []
             ; jump= None } )
    | T.MOVE (T.MEM (T.CONST i), e1) ->
        emit
          (A.OPER
             { assem= "\tsw\t`s0, " ^ string_of_int i ^ "($zero)\n"
             ; src= [munchExp e1]
             ; dst= []
             ; jump= None } )
    | T.MOVE (T.MEM e1, e2) ->
        emit
          (A.OPER
             { assem= "\tsw\t`s0, 0(`s1)\n" (* temp: or (`s1)*)
             ; src= [munchExp e1; munchExp e2]
             ; dst= []
             ; jump= None } )
    | T.MOVE (T.TEMP i, e2) -> emit (A.MOVE {assem= "\tmove\t`d0, `s0\n"; src= munchExp e2; dst= i})
    (*                        ^^pseudo instruction^^
     * register allocator wants Assem.MOVE because it may be redundant.
     * | T.MOVE (T.TEMP i, e2) ->
     *     emit (A.OPER {assem= "\tadd\t`d0, $zero, `s0\n"; src= [munchExp e2]; dst= [i]; jump= None})*)
    | T.MOVE _ -> ErrorMsg.impossible "Invalid MOVE instruction"
    | T.JUMP (T.NAME lab, _) ->
        emit (A.OPER {assem= "\tj\t`j0\n"; src= []; dst= []; jump= Some [lab]})
    | T.JUMP (e, labs) ->
        emit (A.OPER {assem= "\tjr\t`s0\n"; src= [munchExp e]; dst= []; jump= Some labs})
    | T.CJUMP (T.LT, e1, e2, l1, l2) ->
        emit
          (A.OPER
             { assem= "\tblt\t`s0, `s1, `j0\n\tb `j1\n"
             ; dst= []
             ; src= [munchExp e1; munchExp e2]
             ; jump= Some [l1; l2] } )
    | T.CJUMP (T.GT, e1, e2, l1, l2) ->
        emit
          (A.OPER
             { assem= "\tbgt\t`s0, `s1, `j0\n\tb `j1\n"
             ; dst= []
             ; src= [munchExp e1; munchExp e2]
             ; jump= Some [l1; l2] } )
    | T.CJUMP (T.LE, e1, e2, l1, l2) ->
        emit
          (A.OPER
             { assem= "\tble `s0, `s1, `j0\n\tb `j1\n"
             ; dst= []
             ; src= [munchExp e1; munchExp e2]
             ; jump= Some [l1; l2] } )
    | T.CJUMP (T.GE, e1, e2, l1, l2) ->
        emit
          (A.OPER
             { assem= "\tblt\t`s0, `s1, `j0\n\tb `j1\n"
             ; dst= []
             ; src= [munchExp e1; munchExp e2]
             ; jump= Some [l1; l2] } )
    | T.CJUMP (T.EQ, e1, e2, l1, l2) ->
        emit
          (A.OPER
             { assem= "\tbeq\t`s0, `s1, `j0\n\tb `j1\n"
             ; dst= []
             ; src= [munchExp e1; munchExp e2]
             ; jump= Some [l1; l2] } )
    | T.CJUMP (T.NE, e1, e2, l1, l2) ->
        emit
          (A.OPER
             { assem= "\tbne\t`s0, `s1, `j0\n\tb `j1\n"
             ; dst= []
             ; src= [munchExp e1; munchExp e2]
             ; jump= Some [l1; l2] } )
    | T.CJUMP _ -> ErrorMsg.impossible "Tiger compiler does not generate this kind of Tree.CJUMP"
    | T.LABEL lab -> emit (A.LABEL {assem= Symbol.name lab ^ ":\n"; lab})
    | T.EXP (T.CALL (T.NAME lab, args)) ->
        let tmpregs = List.map (fun r -> (Temp.newtemp (), r)) Frame.callersaves in
        let fetch t r = T.MOVE (T.TEMP r, T.TEMP t) in
        let store t r = T.MOVE (T.TEMP t, T.TEMP r) in
        List.iter (fun (t, r) -> munchStm (store t r)) tmpregs;
        emit
          (A.OPER
             { assem= "\tjal\t" ^ Symbol.name lab ^ "\n"
             ; src= munchArgs (0, args)
             ; dst= calldefs
             ; jump= None } );
        List.iter (fun (t, r) -> munchStm (fetch t r)) (List.rev tmpregs)
    | T.EXP e -> ignore (munchExp e)
  and munchExp = function
    | T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)) ->
        result (fun r ->
            emit
              (A.OPER
                 { assem= "\tlw\t`d0, " ^ string_of_int i ^ "(`s0)\n"
                 ; src= [munchExp e1]
                 ; dst= [r]
                 ; jump= None } ) )
    | T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)) ->
        result (fun r ->
            emit
              (A.OPER
                 { assem= "\tlw\t`d0, " ^ string_of_int i ^ "(`s0)\n"
                 ; src= [munchExp e1]
                 ; dst= [r]
                 ; jump= None } ) )
    | T.MEM (T.CONST i) ->
        result (fun r ->
            emit
              (A.OPER
                 { assem= "\tlw\t`d0, " ^ string_of_int i ^ "($zero)\n"
                 ; src= []
                 ; dst= [r]
                 ; jump= None } ) )
    | T.MEM e1 ->
        result (fun r ->
            emit
              (A.OPER
                 { assem= "\tlw\t`d0, 0(`s0)\n" (* temp: or (`s1)*)
                 ; src= [munchExp e1]
                 ; dst= [r]
                 ; jump= None } ) )
    | T.BINOP (T.PLUS, e1, T.CONST i) ->
        result (fun r ->
            emit
              (A.OPER
                 { assem= "\taddiu\t`d0, `s0, " ^ string_of_int i ^ "\n"
                 ; src= [munchExp e1]
                 ; dst= [r]
                 ; jump= None } ) )
    | T.BINOP (T.PLUS, T.CONST i, e1) ->
        result (fun r ->
            emit
              (A.OPER
                 { assem= "\taddiu\t`d0, `s0, " ^ string_of_int i ^ "\n"
                 ; src= [munchExp e1]
                 ; dst= [r]
                 ; jump= None } ) )
    | T.BINOP (T.PLUS, e1, e2) ->
        result (fun r ->
            emit
              (A.OPER
                 { assem= "\tadd\t`d0, `s0, `s1\n"
                 ; src= [munchExp e1; munchExp e2]
                 ; dst= [r]
                 ; jump= None } ) )
    | T.BINOP (T.MINUS, e1, T.CONST i) ->
        result (fun r ->
            emit
              (A.OPER
                 { assem= "\taddiu\t`d0, `s0, " ^ string_of_int (-i) ^ "\n"
                 ; src= [munchExp e1]
                 ; dst= [r]
                 ; jump= None } ) )
    | T.BINOP (T.MINUS, e1, e2) ->
        result (fun r ->
            emit
              (A.OPER
                 { assem= "\tsub\t`d0, `s0, `s1\n"
                 ; src= [munchExp e1; munchExp e2]
                 ; dst= [r]
                 ; jump= None } ) )
    | T.BINOP (T.MUL, e1, e2) ->
        result (fun r ->
            emit
              (A.OPER
                 { assem= "\tmul\t`d0, `s0, `s1\n"
                 ; src= [munchExp e1; munchExp e2]
                 ; dst= [r]
                 ; jump= None } ) )
    | T.BINOP (T.DIV, e1, e2) ->
        result (fun r ->
            emit
              (A.OPER
                 { assem= "\tdiv\t`d0, `s0, `s1\n"
                 ; src= [munchExp e1; munchExp e2]
                 ; dst= [r]
                 ; jump= None } ) )
    | T.BINOP _ -> ErrorMsg.impossible "Tiger compiler does not generate this kind of Tree.BINOP"
    | T.CONST i ->
        result (fun r ->
            emit
              (A.OPER {assem= "\tli\t`d0, " ^ string_of_int i ^ "\n"; src= []; dst= [r]; jump= None}) )
    | T.TEMP t -> t
    | T.NAME lab ->
        result (fun r ->
            emit
              (A.OPER {assem= "\tla\t`d0, " ^ Symbol.name lab ^ "\n"; src= []; dst= [r]; jump= None}) )
    | T.ESEQ _ -> ErrorMsg.impossible "Tree.ESEQ should have been removed in Canon module"
    | T.CALL (T.NAME lab, args) ->
        let tmpregs = List.map (fun r -> (Temp.newtemp (), r)) Frame.callersaves in
        let fetch t r = T.MOVE (T.TEMP r, T.TEMP t) in
        let store t r = T.MOVE (T.TEMP t, T.TEMP r) in
        List.iter (fun (t, r) -> munchStm (store t r)) tmpregs;
        emit
          (A.OPER
             { assem= "\tjal\t" ^ Symbol.name lab ^ "\n"
             ; src= munchArgs (0, args)
             ; dst= calldefs
             ; jump= None } );
        List.iter (fun (t, r) -> munchStm (fetch t r)) (List.rev tmpregs);
        Frame.rv
  and munchArgs = function
    | _, [] -> []
    | i, arg :: rest ->
        let dst = List.nth Frame.argregs i in
        let src = munchExp arg in
        munchStm (T.MOVE (T.TEMP dst, T.TEMP src));
        (* temp: spill *)
        dst :: munchArgs (i + 1, rest)
  in
  munchStm stm; List.rev !ilist
