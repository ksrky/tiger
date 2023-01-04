type allocation = Frame.register Temp.Table.t

type input = { interference: Liveness.igraph
             ; initial: allocation
             ; spillCost : Graph.node -> int
             ; registers: Frame.register list}

module GT = Graph.Table
module GS = Graph.Set
module TT = Temp.Table
module TS = Temp.Set

let n_colors = 8 (* K *)
let initial : Temp.temp list ref = ref []

let simplifyWorklist : Temp.temp list ref = ref []
let freezeWorklist : Temp.temp list ref = ref []
let spillWorklist : Temp.temp list ref = ref []
let spilledNodes : GS.t ref = ref GS.empty
let coalescedNodes : Temp.temp list ref = ref []
let coloredNodes : Temp.temp list ref = ref []
let selectStack : Temp.temp Stack.t = Stack.create()
(*
let coalescedMoves : GS.t ref = ref GS.empty
let worklistMoves : GS.t ref = ref GS.empty (* fnode *)
let activeMoves : GS.t ref = ref GS.empty (* fnode *)*)

let coalescedMoves : (Graph.node * Graph.node) list ref = ref []
let worklistMoves : (Graph.node * Graph.node) list ref = ref []
let activeMoves : (Graph.node * Graph.node) list ref = ref []

let adjSet : (Temp.temp * Temp.temp) list ref = ref []
let adjList : Temp.temp list TT.t ref = ref TT.empty
let degree : int TT.t ref  = ref TT.empty
let moveList : (Graph.node * Graph.node) list TT.t ref = ref TT.empty (* mapping inode to fnode *)
let alias v = List.assoc v !coalescedMoves
let color : Frame.register GT.t ref = ref GT.empty


let color ({interference; initial=_; spillCost; registers} :input) : allocation * Temp.temp list =
  let IGRAPH{graph; tnode; gtemp; moves} = interference in
  let build() =
    worklistMoves := moves;
    List.iter (fun (u, v) ->
      let u' = gtemp u in
      let v' = gtemp v in
      let newMlist = (u, v) :: (TT.find u' !moveList) in
      moveList := TT.add u' newMlist !moveList;
      moveList := TT.add v' newMlist !moveList
      ) moves in

  let nodeMoves(n : Temp.temp) =
    List.filter (fun mv -> not(List.mem mv (!activeMoves @ !worklistMoves)))
      (TT.find n !moveList) in
  let moveRelated(n) = nodeMoves(n) <> [] in
  let makeWorklist() =
    List.iter (fun n ->
      if TT.find n !degree >= n_colors then
        spillWorklist := n :: !spillWorklist
      else if moveRelated(n) then
        freezeWorklist := n :: !freezeWorklist
      else
        simplifyWorklist := n :: !simplifyWorklist) !initial in
  
  let adjacent(n) =
    let selectStack' = List.of_seq(Stack.to_seq selectStack) in
    List.filter (fun m -> not(List.mem m (selectStack' @ !coalescedNodes))) (TT.find n !adjList) in
  let enableMoves(nodes) =
    List.iter (fun n ->
      List.iter (fun m ->
        if List.mem m !activeMoves then
          activeMoves := List.filter ((<>) m) !activeMoves;
          worklistMoves := m :: !worklistMoves) (nodeMoves n)) nodes in
  let decrementDegree(m : Temp.temp) =
    let d = TT.find m !degree in
    degree := TT.add m (d-1) !degree;
    if d = n_colors then
      enableMoves(adjacent(m));
      spillWorklist := List.filter ((<>) m) !spillWorklist;
      if moveRelated(m)
        then freezeWorklist := m :: !freezeWorklist
        else simplifyWorklist := m :: !simplifyWorklist in
      
  let simplify() =
    List.iter (fun n ->
      Stack.push n selectStack;
      List.iter (fun m -> decrementDegree(m)) (adjacent n)
      ) !simplifyWorklist in
  
  let rec repeat() =
    let terminate = ref false in
    (if !simplifyWorklist <> [] then simplify()
    else terminate := true);
    if !terminate then () else repeat() in

  let rewriteProgram() = () in
  
  let rec main() : unit =
    build();
    makeWorklist();
    repeat();
    if !spilledNodes <> GS.empty
      then (rewriteProgram(); main())
      else () in

  main();
  raise ErrorMsg.Error

(*

let (++) : Graph.Set.t -> Graph.Set.t -> Graph.Set.t =Graph.Set.union
let (--) : Graph.Set.t -> Graph.Set.t -> Graph.Set.t = Graph.Set.diff*)


let rec getAlias n =
  if List.mem n !coalescedNodes
    then getAlias(alias n)
  else n  

  (*
let assignColor ({interference; initial; spillCost; registers}) =
  let repeat() =
    let n = Stack.pop selectStack in
    let okColors = ref (List.init (n_colors-1) (fun x -> x)) in
    List.iter (fun w ->
      if Graph.Set.mem (getAlias w) (!coloredNodes ++ precolored)
        then okColors := List.filter (fun c -> c <> color(getAlias(w))) !okColors 
        else ()) (Temp.Table.find n !adjList);
    if !okColors = []
      then spilledNodes := !spilledNodes ++ (Graph.Set.return n)
      else coloredNodes := !coloredNodes ++ (Graph.Set.return n);
           let (c :: _) = okColors in
           color := Graph.Table.add n c !color;
  List.iter (fun n ->
    let new_color = Graph.Table.find (getAlias n) !color in
    color := Graph.Table.add n new_color !color) !coalescedNodes in
  (initial, []*)