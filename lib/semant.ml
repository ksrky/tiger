module A = Absyn
module E = Env
module S = Symbol
module T = Types
module Tr = Translate

(*type venv = E.enventry S.table
type tenv = T.ty S.table*)

type expty = {exp: Translate.exp; ty: T.ty}

let error = ErrorMsg.error
let err_expty = {exp=Tr.nilExp; ty=T.NIL}

let rec check_type(ty, exp_ty, pos) =
  let ty' = actual_ty(ty, pos) in
  let exp_ty' = actual_ty(exp_ty, pos) in
  if ty' = exp_ty' || T.NIL = ty' || T.NIL = exp_ty'
    then ()
    else error pos ("type mismatch. " ^ "expected " ^ T.type2str exp_ty
      ^ ", but got " ^ T.type2str ty)

and actual_ty(ty, pos) =
  let rec walk1 ty = match ty with
    | T.NAME(id, ref) -> (match !ref with
      | Some(ty) -> walk1 ty
      | None -> error pos ("type not found " ^ S.name id); T.NIL)
    | _ -> ty
  in walk1 ty

let rec transExp(venv, tenv, level, breakpoint, exp) : expty =
  let rec trexp = function
    | A.VarExp var -> trvar var
    | A.NilExp -> {exp=Tr.nilExp; ty=T.NIL}
    | A.IntExp i -> {exp=Tr.intExp i; ty=T.INT}
    | A.StringExp(s, _) -> {exp=Tr.stringExp s; ty=T.STRING}
    | A.CallExp{func; args; pos} ->
      (match S.look(venv, func) with
        | None -> error pos ("undefined function " ^ S.name func); err_expty
        | Some (E.VarEntry _) -> error pos "expecting a function, not a variable"; err_expty
        | Some (E.FunEntry{level=cal_lev; label; formals; result}) ->
            let args_exp = checkformals(args, formals, pos) in
            {exp=Tr.callExp(level, cal_lev, label, args_exp); ty=result})
    | A.OpExp{left; oper; right; pos} ->
      let {exp=left_exp; ty=left_ty} = trexp left in
      let {exp=right_exp; ty=right_ty} = trexp right in
      let ops = [(A.PlusOp, Tr.plusExp); (A.MinusOp, Tr.minusExp); (A.TimesOp, Tr.timesExp);
                 (A.DivideOp, Tr.divideExp); (A.LtOp, Tr.ltExp); (A.GtOp, Tr.gtExp);
                 (A.LeOp, Tr.leExp); (A.GeOp, Tr.geExp); (A.EqOp, Tr.eqExp); (A.NeqOp, Tr.neqExp)] in
      let res_exp = (List.assoc oper ops) left_exp right_exp in
      (match (left_ty, right_ty) with
        | (T.INT, T.INT) -> {exp=res_exp; ty=T.INT}
        | (T.STRING, T.STRING) ->
          if List.mem oper [PlusOp; MinusOp; TimesOp; DivideOp]
            then (error pos "invalid comparison for string"; err_expty)
            else {exp=res_exp; ty=T.INT}
        | (ty, ty') when ty = ty' ->
          if List.mem oper [EqOp; NeqOp]
            then {exp=res_exp; ty=T.INT}
            else (error pos "invalid comparison for "; err_expty)
        | _ -> error pos "comparison of incompatible types"; err_expty)
    | A.RecordExp{fields; typ; pos} ->
      (match S.look(tenv, typ) with
        | None -> error pos ("type not found " ^ S.name typ); err_expty
        | Some(rec_ty) -> 
          let rec_ty' = actual_ty(rec_ty, pos) in
          match rec_ty' with
            | T.RECORD(field_tys, _) ->
              let fields_exp = checkrecord(fields, field_tys) in
              {exp=Tr.recordExp fields_exp; ty=rec_ty}
            | _ -> error pos "variable not a record"; err_expty)
    | A.SeqExp seqexp ->
      (let res_ty = ref T.UNIT in
      let rec trexps = function
        | [] -> []
        | [(e, _)] -> res_ty := (trexp e).ty; [(trexp e).exp]
        | ((e, _) :: se) -> (trexp e).exp :: trexps se in
      {exp=Tr.seqExp (trexps seqexp); ty=(!res_ty)})
    | A.AssignExp{var; exp; pos} ->
      let {exp=var_exp; ty=var_ty} = trvar var in
      let {exp=exp_exp; ty=exp_ty} = trexp exp in
      check_type(var_ty, exp_ty, pos); {exp=Tr.assignExp(var_exp, exp_exp); ty=T.UNIT}
    | A.IfExp{test; then'; else'; pos} ->
      let {exp=test_exp; ty=test_ty} = trexp test in
      check_int(test_ty, pos);
      let {exp=then_exp; ty=then_ty} = trexp then' in
      (match else' with
        | None ->
          check_type(T.UNIT, then_ty, pos);
          {exp=Tr.ifThenExp test_exp then_exp; ty=T.UNIT}
        | Some(else'') ->
          let {exp=else_exp; ty=else_ty} = trexp else'' in
          check_type(then_ty, else_ty, pos);
          {exp=Tr.ifThenElseExp test_exp then_exp else_exp; ty=then_ty})
    | A.WhileExp{test; body; pos} ->
      let {exp=test_exp; ty=test_ty} = trexp test in
      check_int(test_ty, pos);
      let breakpoint = Temp.newlabel() in
      let {exp=body_exp; ty=body_ty} = transExp(venv, tenv, level, Some(breakpoint), body) in
      check_type(T.UNIT, body_ty, pos);
      {exp=Tr.whileExp(test_exp, body_exp, breakpoint); ty=T.UNIT}
    | A.ForExp{var; escape; lo; hi; body; pos} ->
      let i = A.SimpleVar (var, pos) in
      let limit = S.symbol "$limit" in
      let decs = [
        A.VarDec{name=var; escape; typ=Some(Symbol.symbol "int", pos); init=lo; pos};
        A.VarDec{name=limit; escape; typ=Some(Symbol.symbol "int", pos); init=hi; pos}] in
      let body = A.WhileExp{test=A.OpExp {left=A.VarExp i;
                                          oper=A.LeOp;
                                          right=A.VarExp (A.SimpleVar (limit, pos));
                                          pos};
                            body=A.SeqExp [
                              (body, pos);
                              (A.AssignExp{var=i;
                                          exp=A.OpExp{left=A.VarExp i; oper=A.PlusOp; right=IntExp 1; pos};
                                          pos}, pos)];
                            pos} in
      trexp (A.LetExp{decs; body; pos})
    | A.BreakExp(pos) ->
      (match breakpoint with
        | Some(bp) -> {exp=Tr.breakExp bp; ty=T.UNIT}
        | None -> error pos "`break` must be in a `for` or `while` expression";
                  err_expty)
    | A.LetExp{decs; body; _} ->
      let (venv', tenv') = transDecs(venv, tenv, level, breakpoint, decs) in
      transExp(venv', tenv', level, breakpoint, body)
    | A.ArrayExp{typ; size; init; pos} ->
      match S.look(tenv, typ) with
        | None -> error pos ("type not found " ^ S.name typ); err_expty
        | Some(arr_ty) ->
          let arr_ty' = actual_ty(arr_ty, pos) in
          (match arr_ty' with
            | T.ARRAY(ty, _) ->
              let {exp=size_exp; ty=size_ty} = trexp size in
              check_int(size_ty, pos);
              let {exp=init_exp; ty=init_ty} = trexp init in
              check_type(ty, init_ty, pos);
              {exp=Tr.arrayExp size_exp init_exp; ty=arr_ty}
            | _ -> error pos "variable not an array"; err_expty)
  and trvar = function
    | A.SimpleVar(id, pos) -> (match S.look(venv, id) with
      | Some(E.VarEntry{access; ty}) -> {exp=Tr.simpleVar(access, level); ty= ty}
      | Some(_) -> error pos "expecting a variable, not a function"; err_expty
      | None -> error pos ("undefined variable " ^ S.name id); err_expty)
    | A.FieldVar(var, id, pos) ->
      let {exp;ty} = trvar var in
      (match ty with
        | T.RECORD(fields, _) ->
          let rec find i = function
            | [] -> error pos ("label not found " ^ S.name id); (-1, T.NIL)
            | ((id', ty') :: rest) -> if id = id' then (i, ty') else find (i+1) rest in
          let (idx, ty') =  find 0 fields in
          {exp=Tr.fieldVar(exp, idx); ty=ty'}
        | _ -> error pos "expecting a record"; err_expty)
    | A.SubscriptVar(var, exp, pos) ->
      let {exp=var_exp; ty=var_ty} = trvar var in
      match var_ty with
        | ARRAY(ty,_) ->
          let {exp=exp_exp; ty=exp_ty} = trexp exp
          in check_int(exp_ty, pos); {exp=Tr.subscriptVar(var_exp, exp_exp); ty}
        | _ -> error pos "expecting an array"; err_expty
  and check_int(ty, pos) = match ty with
    | T.INT -> ()
    | _ -> error pos "expecting int type"
  and checkformals : (A.exp list*T.ty list*A.pos) -> Tr.exp list = function
    | ([], [], _) -> []
    | (e::es, ty::tys, pos) ->
      let {exp=e'; ty=e_ty} = trexp e
    in check_type(ty, e_ty, pos); e'::(checkformals(es, tys, pos))
    | (_, _, pos) -> error pos ""; []
  and checkrecord = function
    | ([], _) -> []
    | ((lab, e, pos)::fields, field_tys) ->
      let ty = try List.assoc lab field_tys with
        | Not_found -> error pos ("label not found " ^ S.name lab); T.NIL
      in let {exp; ty=e_ty} = trexp e in
      check_type(ty, e_ty, pos); exp::checkrecord(fields, field_tys)
in trexp exp

and transDecs(venv, tenv, level, breakpoint, decs) =
  let transDec (venv, tenv) = function
    | A.TypeDec tydecs ->
      let rec check_name_uniq : A.typedec list -> unit = function
        | [] -> ()
        | {A.name=id;pos;_}::rest ->
          try
            let {A.name=id2;_} = List.find (fun {A.name=id2;_} -> S.name id = S.name id2) rest in
            error pos ("multiple declaration for " ^ S.name id2)
          with
            Not_found -> check_name_uniq rest in
      let check_cycle tydecs =
        let rec walk deps name =
          try
            let ty = List.find (fun {A.name=name';_} -> name = name') tydecs in
            match ty with
              | {ty=A.NameTy(typ, pos);_} -> 
                if List.exists ((=) typ) deps
                  then error pos "cyclic dependencies"
                  else walk (name :: deps) typ
              | _ -> ()
          with
            Not_found -> () 
        in List.iter (fun {A.name;_} -> walk [] name) tydecs in
      let tenv' = List.fold_left (fun env {A.name;_} ->
        S.enter(env, name, T.NAME(name, ref None))) tenv tydecs in
      let tenv'' = List.fold_left (fun env {A.name; A.ty; _} ->
        (match S.look(env, name) with
          | Some(T.NAME(_, ref)) -> ref := Some(transTy(env, ty))
          | _ -> ((*unreachable*))); env) tenv' tydecs in
      check_name_uniq tydecs;
      check_cycle tydecs;
      (venv, tenv'')
    | A.FunctionDec fundecs ->
      let rec check_name_uniq : A.fundec list -> unit = function
        | [] -> ()
        | {A.name=id;pos;_}::rest ->
          try
            let {A.name=id2;_} : A.fundec = List.find (fun ({A.name=id2;_} : A.fundec) ->
              S.name id = S.name id2) rest in
            error pos ("multiple declaration for " ^ S.name id2)
          with
            Not_found -> check_name_uniq rest in
      let transparam{A.name; A.escape; A.typ; A.pos; _} = match S.look(tenv, typ) with
        | Some(ty) -> (name, !escape, ty, pos)
        | None -> error pos ("type not found" ^ S.name typ); (name, true, T.NIL, pos) in
      let transfun_header(venv, {A.name; A.params; A.result; _}) =
        let rec check_name_uniq : A.field list -> unit = function
          | [] -> ()
          | {A.name=id;pos;_}::rest ->
            try
              let {A.name=id2;_} : A.field = List.find (fun ({A.name=id2;_} : A.field) ->
                S.name id = S.name id2) rest in
              error pos ("multiple declaration for " ^ S.name id2)
            with
              Not_found -> check_name_uniq rest in
        let formals = List.map (fun {A.typ; A.pos; _} -> match S.look(tenv, typ) with
          | Some(ty) -> ty
          | None -> error pos ("type not found" ^ S.name typ); T.NIL) params in
        let res_ty = match result with
          | Some(typ, pos) ->
            (match S.look(tenv, typ) with
              | Some(res_ty) -> res_ty
              | None -> error pos ("type not found" ^ S.name typ); T.NIL)
          | None -> T.UNIT (*no type annotation*) in
        check_name_uniq params;
        S.enter(venv, name, E.FunEntry{level=Tr.newLevel(level, name, []);
                                      label=name;
                                      formals;
                                      result=res_ty}) in
      let venv' = List.fold_left (fun env fundec ->
          transfun_header(env, fundec)) venv fundecs in
      let transfun_body{A.name; A.params; A.result; A.body; _} =
        match S.look(venv', name) with
          | Some(E.FunEntry{level=newlevel;_}) ->
            let params' = List.map transparam params in
            let venv'' = List.fold_left (fun env (id, esc, ty, _) ->
              let access = Tr.allocLocal(level, esc) in
              S.enter(env, id, E.VarEntry{access; ty})) venv' params' in
            let {exp=_; ty=body_ty} = transExp(venv'', tenv, newlevel, breakpoint, body) in
              (match result with
                | Some(typ, pos) ->
                  (match S.look(tenv, typ) with
                    | Some(res_ty) -> check_type(res_ty, body_ty, pos)
                    | None -> error pos ("type not found" ^ S.name typ))
                | None -> ())
          | _ -> ErrorMsg.impossible "" in
      check_name_uniq fundecs;
      List.iter transfun_body fundecs;
      (venv', tenv)
    | A.VarDec{name; escape; typ=None; init; _} ->
      let {exp=_; ty=init_ty} = transExp(venv, tenv, level, breakpoint, init) in
      let access = Tr.allocLocal(level, !escape) in
      (S.enter(venv, name, E.VarEntry{access; ty=init_ty}), tenv)
    | A.VarDec{name; escape; typ=Some(typ, _); init; pos; _} ->
      let {exp=_; ty=init_ty} = transExp(venv, tenv, level, breakpoint, init) in
      let access = Tr.allocLocal(level, !escape) in
      match S.look(tenv, typ) with
        | None -> error pos ("type not found " ^ S.name typ); (venv, tenv)
        | Some(res_ty) ->
          check_type(res_ty, init_ty, pos);
          (S.enter(venv, name, E.VarEntry{access; ty=init_ty}), tenv)
  in List.fold_left transDec (venv, tenv) decs

and transTy(tenv, ty) = match ty with
  | A.NameTy(typ, pos) ->
    (match S.look(tenv, typ) with
      | Some(ty) -> ty
      | None -> error pos ("type not found " ^ S.name typ); T.NIL)
  | A.RecordTy fields ->
    let field_tys = List.map (fun {A.name; typ; pos; _} ->
      (match S.look(tenv, typ) with
        | Some(ty) -> (name, ty)
        | None -> error pos ("type not found " ^ S.name typ); (name, T.NIL))) fields in
    T.RECORD(field_tys, ref())
  | A.ArrayTy(typ, pos) ->
    (match S.look(tenv, typ) with
      | Some(ty) -> T.ARRAY(ty, ref())
      | None -> error pos ("type not found " ^ S.name typ); T.NIL)  

let transProg exp : Frame.frag list =
  let mainlevel = Tr.newLevel(Tr.outermost, Temp.namedlabel "main", []) in
  let {exp; _} = transExp(E.base_venv, E.base_tenv, mainlevel, None, exp) in
  Tr.procEntryExit(mainlevel, exp);
  Tr.getResult()