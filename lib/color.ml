type allocation = Frame.register Temp.Table.t

module GT = Graph.Table
module GS = Graph.Set
module TT = Temp.Table
module TS = Temp.Set

module TTS = Set.Make (struct
  type t = Temp.temp * Temp.temp

  let compare = Stdlib.compare
end)

let n_colors = 8 (* K *)

let color (interference, init_alloc, _spillCost, registers) : allocation * Temp.temp list =
  (* nodes list *)
  let precolored : Temp.temp list ref = ref [] in
  let initial : Temp.temp list ref = ref [] in
  let simplifyWorklist : TS.t ref = ref TS.empty in
  let freezeWorklist : TS.t ref = ref TS.empty in
  let spillWorklist : TS.t ref = ref TS.empty in
  let spilledNodes : Temp.temp list ref = ref [] in
  let coalescedNodes : Temp.temp list ref = ref [] in
  let coloredNodes : Temp.temp list ref = ref [] in
  let selectStack : Temp.temp Stack.t = Stack.create () in
  (* moves list *)
  let coalescedMoves : TTS.t ref = ref TTS.empty in
  let constrainedMoves : (Temp.temp * Temp.temp) list ref = ref [] in
  let frozenMoves : (Temp.temp * Temp.temp) list ref = ref [] in
  let worklistMoves : TTS.t ref = ref TTS.empty in
  let activeMoves : TTS.t ref = ref TTS.empty in
  (* other data structures *)
  let adjSet : (Temp.temp * Temp.temp) list ref = ref [] in
  let adjList : Temp.temp list TT.t ref = ref TT.empty in
  let degree : int TT.t ref = ref TT.empty in
  let moveList : TTS.t TT.t ref = ref TT.empty in
  let alias : Temp.temp TT.t ref = ref TT.empty in
  let color : Frame.register TT.t ref = ref TT.empty in
  (* interference graph *)
  let (Liveness.IGRAPH {graph; tnode= _; gtemp; moves}) = interference in
  (* initialize *)
  let () =
    color := init_alloc;
    let temps = List.map gtemp (Graph.nodes graph) in
    List.iter
      (fun t ->
        if TT.mem t init_alloc then precolored := t :: !precolored else initial := t :: !initial;
        degree := TT.add t 0 !degree;
        moveList := TT.add t TTS.empty !moveList;
        adjList := TT.add t [] !adjList )
      temps
  in
  (* build graph *)
  let rec build () =
    List.iter
      (fun (u, v) ->
        let u' = gtemp u in
        let v' = gtemp v in
        let newMlist = TTS.add (u', v') (TT.find u' !moveList) in
        moveList := TT.add u' newMlist !moveList;
        moveList := TT.add v' newMlist !moveList;
        worklistMoves := TTS.add (u', v') !worklistMoves )
      moves;
    let nodes = Graph.nodes graph in
    List.iter (fun u -> List.iter (fun v -> addEdge (gtemp u, gtemp v)) (Graph.adj u)) nodes
  and addEdge (u, v) =
    if (not (List.mem (u, v) !adjSet)) && u <> v then adjSet := (u, v) :: (v, u) :: !adjSet;
    if not (List.mem u !precolored) then (
      adjList := TT.add u (v :: TT.find u !adjList) !adjList;
      degree := TT.add u (TT.find u !degree + 1) !degree );
    if not (List.mem v !precolored) then (
      adjList := TT.add v (u :: TT.find v !adjList) !adjList;
      degree := TT.add v (TT.find v !degree + 1) !degree )
  in
  (* make worklist *)
  let rec makeWorklist () =
    List.iter
      (fun n ->
        if TT.find n !degree >= n_colors then spillWorklist := TS.add n !spillWorklist
        else if moveRelated n then freezeWorklist := TS.add n !freezeWorklist
        else simplifyWorklist := TS.add n !simplifyWorklist )
      !initial;
    initial := []
  and adjacent (n : Temp.temp) =
    let selectStack' = List.of_seq (Stack.to_seq selectStack) in
    List.filter (fun m -> not (List.mem m (selectStack' @ !coalescedNodes))) (TT.find n !adjList)
  and nodeMoves (n : Temp.temp) =
    TTS.inter (TT.find n !moveList) (TTS.union !activeMoves !worklistMoves)
  and moveRelated (n : Temp.temp) = not (TTS.is_empty (nodeMoves n)) in
  (* simplify *)
  let rec simplify () =
    let n = TS.choose !simplifyWorklist in
    simplifyWorklist := TS.remove n !simplifyWorklist;
    Stack.push n selectStack;
    List.iter decrementDegree (adjacent n)
  and decrementDegree (m : Temp.temp) =
    let d = TT.find m !degree in
    degree := TT.add m (d - 1) !degree;
    if d = n_colors then enableMoves (m :: adjacent m);
    spillWorklist := TS.remove m !spillWorklist;
    if moveRelated m then freezeWorklist := TS.add m !freezeWorklist
    else simplifyWorklist := TS.add m !simplifyWorklist
  and enableMoves nodes =
    List.iter
      (fun n ->
        TTS.iter
          (fun m ->
            if TTS.mem m !activeMoves then activeMoves := TTS.remove m !activeMoves;
            worklistMoves := TTS.add m !worklistMoves )
          (nodeMoves n) )
      nodes
  in
  (* coalesce *)
  let rec coalesce () =
    let ((x, y) as m) = TTS.choose !worklistMoves in
    let x' = getAlias x in
    let y' = getAlias y in
    let u, v = if List.mem y !precolored then (y', x') else (x', y') in
    worklistMoves := TTS.remove m !worklistMoves;
    if u = v then (
      coalescedMoves := TTS.add m !coalescedMoves;
      addWorklist u )
    else if List.mem v !precolored || List.mem (u, v) !adjSet then (
      constrainedMoves := m :: !constrainedMoves;
      addWorklist u;
      addWorklist v )
    else if
      (List.mem u !precolored && List.for_all (fun t -> ok (t, u)) (adjacent v))
      || ((not (List.mem u !precolored)) && conservative (adjacent u @ adjacent v))
    then (
      coalescedMoves := TTS.add m !coalescedMoves;
      combine (u, v);
      addWorklist u )
    else activeMoves := TTS.add m !activeMoves
  and addWorklist (u : Temp.temp) =
    if (not (List.mem u !precolored)) && (not (moveRelated u)) && TT.find u !degree < n_colors then (
      freezeWorklist := TS.remove u !freezeWorklist;
      simplifyWorklist := TS.add u !simplifyWorklist )
  and ok (t, r) : bool =
    TT.find t !degree < n_colors || List.mem t !precolored || List.mem (t, r) !adjSet
  and conservative (nodes : Temp.temp list) : bool =
    let k = ref 0 in
    List.iter (fun n -> if TT.find n !degree >= n_colors then k := !k + 1) nodes;
    !k < n_colors
  and getAlias (n : Temp.temp) =
    if List.mem n !coalescedNodes then getAlias (TT.find n !alias) else n
  and combine (u, v) =
    if TS.mem v !freezeWorklist then freezeWorklist := TS.remove u !freezeWorklist
    else spillWorklist := TS.remove v !spillWorklist;
    coalescedNodes := v :: !coalescedNodes;
    alias := TT.add v u !alias;
    moveList := TT.add u (TTS.union (TT.find u !moveList) (TT.find v !moveList)) !moveList;
    enableMoves [v];
    List.iter
      (fun t ->
        addEdge (t, u);
        decrementDegree t )
      (adjacent v);
    if TT.find u !degree >= n_colors && TS.mem u !freezeWorklist then (
      freezeWorklist := TS.remove u !freezeWorklist;
      spillWorklist := TS.add u !spillWorklist )
  in
  (* freeze *)
  let rec freeze () =
    let u = TS.choose !freezeWorklist in
    freezeWorklist := TS.remove u !freezeWorklist;
    spillWorklist := TS.add u !spillWorklist;
    freezeMoves u
  and freezeMoves (u : Temp.temp) =
    TTS.iter
      (fun ((x, y) as m) ->
        let v = if getAlias y = getAlias u then getAlias x else getAlias y in
        activeMoves := TTS.remove m !activeMoves;
        frozenMoves := m :: !frozenMoves;
        if TTS.is_empty (nodeMoves v) && TT.find v !degree < n_colors then (
          freezeWorklist := TS.remove v !freezeWorklist;
          simplifyWorklist := TS.add v !simplifyWorklist ) )
      (nodeMoves u)
  in
  (* spill *)
  let selectSpill () =
    let m = TS.choose !spillWorklist in
    (* TODO: avoid choosing registers which were spilled before *)
    spillWorklist := TS.remove m !spillWorklist;
    simplifyWorklist := TS.add m !simplifyWorklist;
    freezeMoves m
  in
  (* assign color *)
  let assignColors () =
    let rec repeat () =
      if Stack.is_empty selectStack then ()
      else
        let n = Stack.pop selectStack in
        let okColors = ref registers in
        List.iter
          (fun w ->
            if List.mem (getAlias w) (!coloredNodes @ !precolored) then
              okColors := List.filter (( <> ) (TT.find (getAlias w) !color)) !okColors )
          (Temp.Table.find n !adjList);
        if !okColors = [] then spilledNodes := n :: !spilledNodes
        else (
          coloredNodes := n :: !coloredNodes;
          let (c :: _) = !okColors in
          color := TT.add n c !color );
        repeat ()
    in
    repeat ();
    List.iter
      (fun n ->
        let new_color = TT.find (getAlias n) !color in
        color := TT.add n new_color !color )
      !coalescedNodes
  in
  let rec repeat () =
    let terminate = ref false in
    if TS.is_empty !simplifyWorklist then simplify ()
    else if TTS.is_empty !worklistMoves then coalesce ()
    else if TS.is_empty !freezeWorklist then freeze ()
    else if TS.is_empty !spillWorklist then selectSpill ()
    else terminate := true;
    if !terminate then () else repeat ()
  in
  build (); makeWorklist (); repeat (); assignColors (); (!color, !spilledNodes)
