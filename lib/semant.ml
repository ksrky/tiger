module A = Absyn
module E = Env
module S = Symbol
module T = Types
module TL = Translate

(*type venv = E.enventry S.table
type tenv = T.ty S.table*)

type expty = {exp: Translate.exp; ty: T.ty}

let error = Errormsg.error
let err_expty = {exp=(); ty=T.NIL}

exception Semant

let rec check_type(tenv, ty, exp_ty, pos) =
  let ty' = actual_ty(tenv, ty, pos) in
  let exp_ty' = actual_ty(tenv, exp_ty, pos) in
  if ty' = exp_ty' || T.NIL = ty' || T.NIL = exp_ty'
    then ()
    else error pos ("type mismatch. " ^ "expected " ^ T.type2str exp_ty
      ^ ", but got" ^ T.type2str ty)

and actual_ty(tenv, ty, pos) =
  let rec walk1 ty = match ty with
    | T.NAME(id, _) -> (match S.look(tenv, id) with
      | Some(ty') -> walk1 ty'
      | None -> error pos ""; T.NIL)
    | T.ARRAY(ty', uniq) -> T.ARRAY (walk2 ty', uniq)
    | T.RECORD(fields, uniq) ->
        T.RECORD (List.map (fun (lab, ty) -> (lab, walk2 ty)) fields, uniq)
    | _ -> ty
  and walk2 ty = match ty with
    (* to avoid inifinite loop. eg. `type intlist={hd:int,tl:intlist}`*)
    | T.NAME _ -> ty
    | T.ARRAY(ty', uniq) -> T.ARRAY (walk2 ty', uniq)
    | T.RECORD(fields, uniq) ->
        T.RECORD (List.map (fun (lab, ty) -> (lab, walk2 ty)) fields, uniq)
        | _ -> ty
  in walk1 ty

let rec transExp(venv, tenv, level, exp) : expty =
  let rec trexp = function
    | A.VarExp var -> trvar var
    | A.NilExp -> {exp=(); ty=T.NIL}
    | A.IntExp _ -> {exp=(); ty=T.INT}
    | A.StringExp(_) -> {exp=(); ty=T.STRING}
    | A.CallExp{func; args; pos} ->
      (match S.look(venv, func) with
        | None -> error pos ("undefined function " ^ S.name func); err_expty
        | Some (E.VarEntry _) -> error pos "expecting a function, not a variable"; err_expty
        | Some (E.FunEntry{formals; result; _}) ->
            checkformals(args, formals, pos);
            {exp=(); ty=result})
    | A.OpExp{left; oper; right; pos} ->
      let {exp=_; ty=left_ty} = trexp left in
      let {exp=_; ty=right_ty} = trexp right in
      (match (left_ty, right_ty) with
        | (T.INT, T.INT) -> {exp=(); ty=T.INT}
        | (T.STRING, T.STRING) ->
          if List.mem oper [PlusOp; MinusOp; TimesOp; DivideOp]
            then (error pos "invalid comparison for string"; err_expty)
            else {exp=(); ty=T.INT}
        | (ty, ty') when ty = ty' ->
          if List.mem oper [EqOp; NeqOp]
            then {exp=(); ty=T.INT}
            else (error pos "invalid comparison for "; err_expty)
        | _ -> error pos "comparison of incompatible types"; err_expty)
    | A.RecordExp{fields; typ; pos} ->
      (match S.look(tenv, typ) with
        | None -> error pos ("type not found " ^ S.name typ); err_expty
        | Some(rec_ty) -> 
          let rec_ty' = actual_ty(tenv, rec_ty, pos) in
          match rec_ty' with
            | T.RECORD(field_tys, _) -> checkrecord(fields, field_tys); {exp=(); ty=rec_ty}
            | _ -> error pos "variable not a record"; err_expty)
    | A.SeqExp seqexp ->
      let rec trexps = function
        | [] -> {exp=(); ty=T.UNIT}
        | [(e, _)] -> trexp e
        | ((e, _) :: se) -> ignore (trexp e); trexps se
      in trexps seqexp
    | A.AssignExp{var; exp; pos} ->
      let {exp=_; ty=var_ty} = trvar var in
      let {exp=_; ty=exp_ty} = trexp exp in
      check_type(tenv, var_ty, exp_ty, pos); {exp=(); ty=T.UNIT}
    | A.IfExp{test; then'; else'; pos} ->
      let {exp=_; ty=test_ty} = trexp test in
      check_int(test_ty, pos);
      let {exp=(); ty=then_ty} = trexp then' in
      (match else' with
        | None -> check_type(tenv, T.UNIT, then_ty, pos); {exp=(); ty=T.UNIT}
        | Some(else'') ->
          let {exp=_; ty=else_ty} = trexp else'' in
          check_type(tenv, then_ty, else_ty, pos); {exp=(); ty=then_ty})
    | A.WhileExp{test; body; pos} ->
      let {exp=_; ty=test_ty} = trexp test in
      check_int(test_ty, pos);
      let {exp=(); ty=body_ty} = trexp body in
      check_type(tenv, T.UNIT, body_ty, pos);
      {exp=(); ty=T.UNIT}
    | A.ForExp{var=_; escape=_; lo; hi; body; pos} ->
      let {exp=_; ty=lo_ty} = trexp lo in
      check_int(lo_ty, pos);
      let {exp=_; ty=hi_ty} = trexp hi in
      check_int(hi_ty, pos);
      trexp body
    | A.BreakExp(_) -> raise Semant
    | A.LetExp{decs; body; _} ->
      let (venv', tenv') = transDecs(venv, tenv, level, decs) in
      transExp(venv', tenv', level, body)
    | A.ArrayExp{typ; size; init; pos} ->
      match S.look(tenv, typ) with
        | None -> error pos ("type not found " ^ S.name typ); err_expty
        | Some(arr_ty) ->
          let arr_ty' = actual_ty(tenv, arr_ty, pos) in
          (match arr_ty' with
            | T.ARRAY(ty, _) ->
              let {exp=_; ty=size_ty} = trexp size in
              check_int(size_ty, pos);
              let {exp=_; ty=init_ty} = trexp init in
              check_type(tenv, ty, init_ty, pos);
              {exp=(); ty=arr_ty}
            | _ -> error pos "variable not an array"; err_expty)
  and trvar = function
    | A.SimpleVar(id, pos) -> (match S.look(venv, id) with
      | Some(E.VarEntry{ty; _}) -> {exp=(); ty= ty}
      | Some(_) -> error pos "expecting a variable, not a function";
                  {exp=(); ty=T.INT}
      | None -> error pos ("undefined variable " ^ S.name id);
                {exp=(); ty=T.INT})
    | A.FieldVar(var, id, pos) ->
      let {exp=_;ty} = trvar var in
      (match ty with
        | T.RECORD(fields, _) ->
          let ty' = try List.assoc id fields with
            Not_found -> error pos ("label not found " ^ S.name id); T.INT
          in {exp=(); ty=ty'}
        | _ -> error pos "expecting a record"; {exp=(); ty=T.INT})
    | A.SubscriptVar(var, exp, pos) ->
      let {exp=_;ty} = trvar var in
      match ty with
        | ARRAY(ty',_) ->
          let {exp=_; ty=ty''} = trexp exp
          in check_int(ty'', pos); {exp=(); ty=ty'}
        | _ -> error pos "expecting an array"; {exp=(); ty=T.INT}
  and check_int(ty, pos) = match ty with
    | T.INT -> ()
    | _ -> error pos "expecting int type"
  and checkformals = function
    | ([], [], _) -> ()
    | (e::es, ty::tys, pos) ->
      let {exp=_; ty=e_ty} = trexp e
    in check_type(tenv, ty, e_ty, pos); checkformals(es, tys, pos)
    | (_, _, pos) -> error pos ""
  and checkrecord = function
    | ([], _) -> ()
    | ((lab, e, pos)::fields, field_tys) ->
      let ty = try List.assoc lab field_tys with
        | Not_found -> error pos ("label not found " ^ S.name lab); T.NIL
      in let {exp=_; ty=e_ty} = trexp e in
      check_type(tenv, ty, e_ty, pos); checkrecord(fields, field_tys)
in trexp exp

and transDecs(venv, tenv, level, decs) =
  let transDec (venv, tenv) = function
    | A.TypeDec tydecs ->
      let rec check_name_uniq : A.typedec list -> unit = function
        | [] -> ()
        | {A.name=id;pos;_}::rest ->
          try
            let {A.name=id2;_} = List.find (fun {A.name=id2;_} -> S.name id = S.name id2) rest in
            error pos ("multiple declaration for " ^ S.name id2)
          with
            Not_found -> check_name_uniq (List.tl rest) in
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
            Not_found -> check_name_uniq (List.tl rest) in
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
              Not_found -> check_name_uniq (List.tl rest) in
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
        S.enter(venv, name, E.FunEntry{level=TL.newLevel(level, name, []);
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
              let access = TL.allocLocal(level, esc) in
              S.enter(env, id, E.VarEntry{access; ty})) venv' params' in
            let {exp=_; ty=body_ty} = transExp(venv'', tenv, newlevel, body) in
              (match result with
                | Some(typ, pos) ->
                  (match S.look(tenv, typ) with
                    | Some(res_ty) -> check_type(tenv, res_ty, body_ty, pos)
                    | None -> error pos ("type not found" ^ S.name typ))
                | None -> ())
          | _ -> Errormsg.impossible "" in
      check_name_uniq fundecs;
      List.iter transfun_body fundecs;
      (venv', tenv)
    | A.VarDec{name; escape; typ=None; init; _} ->
      let {exp=_; ty=init_ty} = transExp(venv, tenv, level, init) in
      let access = TL.allocLocal(level, !escape) in
      (S.enter(venv, name, E.VarEntry{access; ty=init_ty}), tenv)
    | A.VarDec{name; escape; typ=Some(typ, _); init; pos; _} ->
      let {exp=_; ty=init_ty} = transExp(venv, tenv, level, init) in
      let access = TL.allocLocal(level, !escape) in
      match S.look(tenv, typ) with
        | None -> error pos ("type not found " ^ S.name typ); (venv, tenv)
        | Some(res_ty) ->
          check_type(tenv, res_ty, init_ty, pos);
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

let transProg exp =
  let mainlevel = TL.newLevel(TL.outermost, Temp.namedlabel "main", []) in
  transExp(E.base_venv, E.base_tenv, mainlevel, exp)