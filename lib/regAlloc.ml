type allocation = Frame.register Temp.Table.t

module GT = Graph.Table
module GS = Graph.Set
module TT = Temp.Table
module TS = Temp.Set

let initial : allocation ref = ref Frame.tempMap

let rec alloc (instrs : Assem.instr list) (frame : Frame.frame) : Assem.instr list * allocation =
  let (FGRAPH {def; use; _} as fgraph), _fnodes = MakeGraph.instr2graph instrs in
  let igraph, _fnode2outs = Liveness.interferenceGraph fgraph in
  let spillCost (n : Graph.node) : int =
    (* TODO: calculate the precedence of spilling nodes. ref: p221 *)
    TS.cardinal (GT.find n def) + TS.cardinal (GT.find n use)
  in
  let allocation, spilledNodes = Color.color (igraph, !initial, spillCost, Frame.registers) in
  let isnt_redundant instr =
    match instr with
    | Assem.MOVE {dst; src; _} -> TT.find dst allocation <> TT.find src allocation
    | _ -> true
  in
  let rewriteProgram () : Assem.instr list =
    let newTemps = ref [] in
    let instrs' =
      List.fold_left
        (fun acc v ->
          let exp = Frame.exp (Frame.allocLocal frame true) (Tree.TEMP Frame.fp) in
          let makeInstr is_def defsrc =
            if List.mem v defsrc then (
              let v' = Temp.newtemp () in
              newTemps := v' :: !newTemps;
              let inst =
                if is_def then Codegen.codegen frame (Tree.MOVE (exp, Tree.TEMP v'))
                else Codegen.codegen frame (Tree.MOVE (Tree.TEMP v', exp))
              in
              let defsrc' = List.filter_map (fun t -> if t = v then Some v' else None) defsrc in
              (inst, defsrc') )
            else ([], defsrc)
          in
          let insertInstr = function
            | Assem.OPER {assem; dst; src; jump} ->
                let store, dst' = makeInstr true dst in
                let fetch, src' = makeInstr false src in
                fetch @ [Assem.OPER {assem; dst= dst'; src= src'; jump}] @ store
            | Assem.MOVE {assem; dst; src} ->
                let store, [dst'] = makeInstr true [dst] in
                let fetch, [src'] = makeInstr false [src] in
                fetch @ [Assem.MOVE {assem; dst= dst'; src= src'}] @ store
            | instr -> [instr]
          in
          List.concat (List.map insertInstr acc) )
        instrs spilledNodes
    in
    instrs'
  in
  if List.length spilledNodes = 0 then (List.filter isnt_redundant instrs, allocation)
  else (
    initial := allocation;
    alloc (rewriteProgram ()) frame )
