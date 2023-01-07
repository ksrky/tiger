type igraph =
  | IGRAPH of
      { graph: Graph.graph
      ; tnode: Temp.temp -> Graph.node
      ; gtemp: Graph.node -> Temp.temp
      ; moves: (Graph.node * Graph.node) list }

type liveSet = Temp.Set.t

type liveMap = liveSet Graph.Table.t

let ( @@ ) (gt : liveSet Graph.Table.t) (n : Graph.node) : liveSet =
  try Graph.Table.find n gt
  with Not_found -> Temp.Set.empty (* node of Assem.Label instruction doesn't have dst or use *)

let ( ++ ) : liveSet -> liveSet -> liveSet = Temp.Set.union

let ( -- ) : liveSet -> liveSet -> liveSet = Temp.Set.diff

let interferenceGraph (Flow.FGRAPH {control; def; use; ismove} : Flow.flowgraph) :
    igraph * (Flow.Graph.node -> Temp.temp list) =
  (* calculate liveness information *)
  let nodes : Flow.Graph.node list = Flow.Graph.nodes control in
  let initLiveInOut =
    List.fold_left
      (fun (liveIn, liveOut) node ->
        (Graph.Table.add node Temp.Set.empty liveIn, Graph.Table.add node Temp.Set.empty liveOut) )
      (Graph.Table.empty, Graph.Table.empty)
      nodes
  in
  let changed : bool ref = ref false in
  let solve (liveIn, liveOut) (node : Flow.Graph.node) : liveMap * liveMap =
    (* note: calculate liveIn after liveOut when solved in reverse order *)
    let oldLiveOut : liveSet = liveOut @@ node in
    let oldLiveIn : liveSet = liveIn @@ node in
    let newLiveOut : liveSet =
      List.fold_left (fun out s -> out ++ (liveIn @@ s)) Temp.Set.empty (Graph.succ node)
    in
    let newLiveIn : liveSet = (use @@ node) ++ newLiveOut -- (def @@ node) in
    if (not (Temp.Set.equal oldLiveIn newLiveIn)) || not (Temp.Set.equal oldLiveOut newLiveOut) then
      changed := true;
    (Graph.Table.add node newLiveIn liveIn, Graph.Table.add node newLiveOut liveOut)
  in
  let rec repeat liveInOut =
    let newLiveInOut = List.fold_left solve liveInOut nodes in
    if !changed then (
      changed := false;
      repeat newLiveInOut )
    else newLiveInOut
  in
  let liveIn, liveOut = repeat initLiveInOut in
  (* make interference graph *)
  let all_temps : Temp.temp list =
    Temp.Set.elements
      (List.fold_left
         (fun ts node -> ts ++ (liveIn @@ node) ++ (liveOut @@ node))
         Temp.Set.empty nodes )
  in
  let graph = Graph.newGraph () in
  let temp2node : (Temp.temp * Graph.node) list =
    List.map
      (fun temp ->
        let node = Graph.newNode graph in
        (temp, node) )
      all_temps
  in
  let moves : (Graph.node * Graph.node) list ref = ref [] in
  let makeMoves fnode : unit =
    if Graph.Table.find fnode ismove then
      let [from] = Temp.Set.elements (use @@ fnode) in
      let [to'] = Temp.Set.elements (def @@ fnode) in
      moves := (List.assoc from temp2node, List.assoc to' temp2node) :: !moves
  in
  let buildGraph (temp, inode) =
    (* fnode: node of FlowGraph, inode: node of InterferenceGraph *)
    List.iter
      (fun fnode ->
        let itf_temps = Temp.Set.elements (liveIn @@ fnode) in
        if List.mem temp itf_temps then
          List.iter
            (fun itf_t ->
              try if temp <> itf_t then Graph.mk_edge (inode, List.assoc itf_t temp2node)
              with Not_found -> ErrorMsg.impossible "at Liveness.interferenceGraph" )
            itf_temps;
        makeMoves fnode )
      nodes
  in
  List.iter buildGraph temp2node;
  let igraph =
    IGRAPH
      { graph
      ; tnode= (fun t -> List.assoc t temp2node)
      ; gtemp= (fun n -> List.assoc n (List.map (fun (x, y) -> (y, x)) temp2node))
      ; moves= !moves }
  in
  let node2outs n = Temp.Set.elements (Graph.Table.find n liveOut) in
  (igraph, node2outs)

let show _ = ()