module A = Absyn
module S = Symbol

type depth = int
type escEnv = (depth * bool ref) S.table

let rec traverseVar(env, d, var) =
  match var with
    | A.SimpleVar(id, _) ->
      (match S.look(env, id) with
        | Some(d', ref) when d > d' -> ref := true
        | _ -> ((*error will be detected in Semant*)))
    | A.FieldVar(var, _, _) -> traverseVar(env, d, var)
    | A.SubscriptVar(var, exp, _) -> traverseVar(env, d, var); traverseExp(env, d, exp)

and traverseExp(env, d, exp) =
  let rec travexp = function
    | A.VarExp(var) -> traverseVar(env, d, var)
    | A.NilExp -> ()
    | A.IntExp(_) -> ()
    | A.StringExp(_) -> ()
    | A.CallExp{args; _} -> List.iter travexp args
    | A.OpExp{left; right; _} -> travexp left; travexp right
    | A.RecordExp{fields; _} -> List.iter (fun (_, e, _) -> travexp e) fields
    | A.SeqExp(seqexp) -> List.iter (fun (e, _) -> travexp e) seqexp
    | A.AssignExp{var; exp; _} -> traverseVar(env, d, var); travexp exp
    | A.IfExp{test; then'; else'; _} -> travexp test; travexp then';
      (match else' with Some(else'') -> travexp else'' | None -> ())
    | A.WhileExp{test; body; _} -> travexp test; travexp body
    | A.ForExp{var; escape; lo; hi; body; _} ->
      let newenv = S.enter(env, var, (d, escape)) in
      escape := false;
      travexp lo; travexp hi;
      traverseExp(newenv, d, body)
    | A.BreakExp(_) -> ()
    | A.LetExp{decs; body; _} ->
      let newenv = traverseDecs(env, d, decs) in
      traverseExp(newenv, d, body)
    | A.ArrayExp{size; init; _} -> travexp size; travexp init
  in travexp exp

and traverseDecs(env, d, decs) =
  match decs with
    | [] -> env
    | dec::decs ->
      let env' = match dec with
        | A.TypeDec(_) -> env
        | A.FunctionDec fundecs ->
          List.iter (fun {A.params; body; _} ->
            let newenv = List.fold_left (fun env {A.name; escape; _} ->
              (escape := false; S.enter(env, name, (d+1, escape)))) env params
            in traverseExp(newenv, d, body)) fundecs; env
        | A.VarDec{name; escape; init; _} -> 
          (escape := false; traverseExp(env, d, init); S.enter(env, name, (d, escape)))
      in traverseDecs(env', d, decs)

let findEscape(exp) = traverseExp(S.empty, 0, exp)