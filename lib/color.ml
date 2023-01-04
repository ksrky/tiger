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

(* worklist, set, stack *)
let precolored = []
let initial : Temp.temp list ref = ref []
let simplifyWorklist : Temp.temp list ref = ref []
let freezeWorklist : Temp.temp list ref = ref []
let spillWorklist : Temp.temp list ref = ref []
let spilledNodes : Temp.temp list ref = ref []
let coalescedNodes : Temp.temp list ref = ref []
let coloredNodes : Temp.temp list ref = ref []
let selectStack : Temp.temp Stack.t = Stack.create()

(* move set *)
let coalescedMoves : (Temp.temp * Temp.temp) list ref = ref []
let constrainedMoves : (Temp.temp * Temp.temp) list ref = ref []
let frozenMoves : (Temp.temp * Temp.temp) list ref = ref []
let worklistMoves : (Temp.temp * Temp.temp) list ref = ref []
let activeMoves : (Temp.temp * Temp.temp) list ref = ref []

(* other data structures *)
let adjSet : (Temp.temp * Temp.temp) list ref = ref []
let adjList : Temp.temp list TT.t ref = ref TT.empty
let degree : int TT.t ref  = ref TT.empty
let moveList : (Temp.temp * Temp.temp) list TT.t ref = ref TT.empty (* mapping inode to fnode *)
let alias : Temp.temp TT.t ref = ref TT.empty
let color : Frame.register TT.t ref = ref TT.empty


let color ({interference; initial=_; spillCost=_; registers} :input) : allocation * Temp.temp list =
  let IGRAPH{graph=_; tnode=_; gtemp; moves} = interference in
  
  let addEdge (u, v) =
    if not (List.mem (u, v) !adjSet) && u <> v then
      adjSet := (u, v) :: (v, u) :: !adjSet;
      (if not (List.mem u precolored) then
        let newAlist = v :: TT.find u !adjList in
        adjList := TT.add u newAlist !adjList;
        degree := TT.add u (TT.find u !degree + 1) !degree);
      if not (List.mem v precolored) then
        let newAlist = u :: TT.find v !adjList in
        adjList := TT.add v newAlist !adjList;
        degree := TT.add v (TT.find v !degree + 1) !degree in
  let build() =
    List.iter (fun (u, v) ->
      let u' = gtemp u in
      let v' = gtemp v in
      let newMlist = (u', v') :: (TT.find u' !moveList) in
      moveList := TT.add u' newMlist !moveList;
      moveList := TT.add v' newMlist !moveList;
      worklistMoves := (u', v') :: !worklistMoves;
      ) moves in

  let nodeMoves(n : Temp.temp) =
    List.filter (fun mv -> not(List.mem mv (!activeMoves @ !worklistMoves)))
      (TT.find n !moveList) in
  let moveRelated(n : Temp.temp) = nodeMoves(n) <> [] in
  let makeWorklist() =
    List.iter (fun n ->
      if TT.find n !degree >= n_colors then
        spillWorklist := n :: !spillWorklist
      else if moveRelated(n) then
        freezeWorklist := n :: !freezeWorklist
      else
        simplifyWorklist := n :: !simplifyWorklist) !initial;
    initial := [] in
  
  let adjacent(n : Temp.temp) =
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
      enableMoves(m :: adjacent(m));
      spillWorklist := List.filter ((<>) m) !spillWorklist;
      if moveRelated(m)
        then freezeWorklist := m :: !freezeWorklist
        else simplifyWorklist := m :: !simplifyWorklist in
  (* simplify *)
  let simplify() =
    List.iter (fun n ->
      Stack.push n selectStack;
      List.iter decrementDegree (adjacent n)
      ) !simplifyWorklist;
    simplifyWorklist := [] in
  
  let addWorklist(u : Temp.temp) =
    if not(List.mem u precolored) && not(moveRelated(u)) &&
      (TT.find u !degree < n_colors) then
        freezeWorklist := List.filter ((<>) u) !freezeWorklist;
        simplifyWorklist := u :: !simplifyWorklist in
  let ok(t, r) : bool =
    TT.find t !degree < n_colors || List.mem t precolored || List.mem (t, r) !adjSet in
  let conservative(nodes : Temp.temp list) : bool =
    let k = ref 0 in
    List.iter (fun n ->
      if TT.find n !degree >= n_colors then k := !k + 1) nodes;
    (!k < n_colors) in
  let rec getAlias(n : Temp.temp) =
    if List.mem n !coalescedNodes
      then getAlias(TT.find n !alias)
    else n in
  let combine(u, v) =
    if List.mem v !freezeWorklist then
      freezeWorklist := List.filter ((<>) u) !freezeWorklist
    else
      spillWorklist := List.filter ((<>) v) !spillWorklist;
      coalescedNodes := v :: !coalescedNodes;
      alias := TT.add v u !alias;
      moveList := TT.add u (TT.find u !moveList @ TT.find v !moveList) !moveList; (* temp: intersect *)
      enableMoves([v]);
      List.iter (fun t ->
        addEdge(t, u);
        decrementDegree(t)) (adjacent v);
      if TT.find u !degree >= n_colors && List.mem u !freezeWorklist then
        freezeWorklist := List.filter ((<>) u) !freezeWorklist;
        spillWorklist := u :: !spillWorklist
    in
  (* coalesce *)
  let coalesce() =
    List.iter (fun ((x, y) as m) ->
      let x' = getAlias(x) in
      let y' = getAlias(y) in
      let (u, v) = if List.mem y precolored
        then (y', x')
        else (x', y') in
      if u = v then
        coalescedMoves := m :: !coalescedMoves
      else if List.mem v precolored || List.mem (u, v) !adjSet then
        (constrainedMoves := m :: !constrainedMoves;
        addWorklist(u);
        addWorklist(v))
      else if List.mem u precolored &&
        List.for_all (fun t -> ok(t, u)) (adjacent(v)) &&
        not(List.mem u precolored) &&
        conservative(adjacent(u) @ adjacent(v)) then (* temp: intersect *)
        (coalescedMoves := m :: !coalescedMoves;
        combine(u, v);
        addWorklist(u))
      else activeMoves := m :: !activeMoves
    ) !worklistMoves in

  let freezeMoves(u : Temp.temp) =
  List.iter (fun ((x, y) as m) ->
    let v = if getAlias(y) = getAlias(u)
      then getAlias(x)
      else getAlias(y) in
    activeMoves := List.filter ((<>) m) !activeMoves;
    frozenMoves := m :: !frozenMoves;
    if nodeMoves(v) = [] && TT.find v !degree < n_colors then
      freezeWorklist := List.filter ((<>) v) !freezeWorklist;
      simplifyWorklist := v :: !simplifyWorklist) (nodeMoves u) in
  (* freezing *)
  let freeze() =
    List.iter (fun u ->
      spillWorklist := u :: !spillWorklist;
      freezeMoves(u)
      ) !freezeWorklist;
    freezeWorklist := [] in
  
  (* spilling *)
  let selectSpill() =
    List.iter (fun m ->
      simplifyWorklist := m :: !simplifyWorklist;
      freezeMoves(m)) !spillWorklist;
    spillWorklist := [] in

  let assignColors () =
    let rec repeat() =
      let n = Stack.pop selectStack in
      let okColors = ref registers in
      List.iter (fun w ->
        if List.mem (getAlias w) (!coloredNodes @ precolored)
          then okColors := List.filter ((<>) (TT.find (getAlias w) !color)) !okColors 
        ) (Temp.Table.find n !adjList);
      if !okColors = [] then
        spilledNodes := n :: !spilledNodes
      else
        coloredNodes := n :: !coloredNodes;
        let c :: _ = !okColors in
        color := TT.add n c !color;
      if Stack.is_empty selectStack then () else repeat() in
    repeat();
    List.iter (fun n ->
      let new_color = TT.find (getAlias n) !color in
      color := TT.add n new_color !color) !coalescedNodes in
    
  
  let rec repeat() =
    let terminate = ref false in
    (if !simplifyWorklist <> [] then simplify()
    else if !worklistMoves <> [] then coalesce()
    else if !freezeWorklist <> [] then freeze()
    else if !spillWorklist <> [] then selectSpill()
    else terminate := true);
    if !terminate then () else repeat() in

  build();
  makeWorklist();
  repeat();
  assignColors();
  (!color, !spilledNodes)