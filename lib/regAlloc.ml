module GT = Graph.Table
module GS = Graph.Set
module TT = Temp.Table
module TS = Temp.Set

type allocation = Frame.register TT.t

let n_colors = 8 (* K *)

let precolored = []
let initial : Temp.temp list = []

let simplifyWorklist : Temp.temp list ref = ref []
let freezeWorklist : Temp.temp list ref = ref []
let spillWorklist : Temp.temp list ref = ref []
let spilledNodes : GS.t ref = ref GS.empty
let coalescedNodes : Temp.temp list ref = ref []
let coloredNodes : Graph.node list ref = ref []
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
let color : int GT.t ref = ref GT.empty

let alloc (ilist: Assem.instr list) (_frame: Frame.frame) : Assem.instr list * allocation =
  let (FGRAPH{control; def; use; ismove} as fgraph, fnodes)
    = MakeGraph.instr2graph ilist in
  let (IGRAPH{graph; tnode; gtemp; moves} as igraph, fnode2outs)
    = Liveness.interferenceGraph fgraph in

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
        simplifyWorklist := n :: !simplifyWorklist) initial in
  
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
    if !spilledNodes <> GS.empty
      then (rewriteProgram(); main())
      else () in

  main();
  raise ErrorMsg.Error




  (*let build() = (* length of fnodes and ilist are same *)
    List.iter (fun fnode ->
      let live = ref (TS.of_list (fnode2outs fnode)) in
      let defn = (GT.find fnode def) in
      let usen = (GT.find fnode use) in
      (if GT.find fnode ismove 
        then
          live := TS.diff !live usen;
          List.iter (fun t ->
            let newMlist = GS.add fnode (TT.find t !moveList) in
            moveList := TT.add t newMlist !moveList)
            (TS.elements(TS.union defn usen));
          worklistMoves := GS.add fnode !worklistMoves);
      live := TS.union !live defn;
      (TS.iter (fun d ->
        TS.iter (fun l ->
          addEdge l d) !live) defn);
      live := TS.union usen (TS.diff !live defn)
      ) fnodes
      
      
      
  let addEdge (u : Temp.temp) (v : Temp.temp) =
    if not (List.mem (u, v) !adjSet) && u <> v then
      adjSet := (u, v) :: (v, u) :: !adjSet;
      (if not (List.mem u precolored) then
        let newAlist = v :: TT.find u !adjList in
        adjList := TT.add u newAlist !adjList;
        degree := TT.add u (TT.find u !degree + 1) !degree);
      if not (List.mem v precolored) then
        let newAlist = u :: TT.find v !adjList in
        adjList := TT.add v newAlist !adjList;
        degree := TT.add v (TT.find v !degree + 1) !degree in*)