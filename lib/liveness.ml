type igraph = IGRAPH of { graph: Graph.graph
                        ; tnode: Temp.temp -> Graph.node
                        ; gtemp: Graph.node -> Temp.temp
                        ; moves: (Graph.node * Graph.node) list}

type liveSet = Temp.Set.t
type liveMap = liveSet Graph.Table.t

let (@@) : liveSet Graph.Table.t -> Graph.node -> liveSet = Fun.flip Graph.Table.find
let (++) : liveSet -> liveSet -> liveSet = Temp.Set.union
let (--) : liveSet -> liveSet -> liveSet = Temp.Set.diff

let liveIn : liveMap ref = ref Graph.Table.empty
let liveOut : liveMap ref = ref Graph.Table.empty

let interferenceGraph (Flow.FGRAPH{control; def; use; ismove}: Flow.flowgraph)
  : igraph * (Flow.Graph.node -> Temp.temp list)
  =
  (* calculate liveness infromation *)
  let nodes : Graph.node list = Flow.Graph.nodes control in
  let init() = List.iter (fun node ->
    liveIn := Graph.Table.add node Temp.Set.empty !liveIn;
    liveOut := Graph.Table.add node Temp.Set.empty !liveOut) nodes in
  let terminate : bool ref = ref true in
  let solve (node : Graph.node) : unit =
    let oldLiveIn : liveSet = !liveIn @@ node  in
    let oldLiveOut : liveSet = !liveOut @@ node in
    let newLiveIn : liveSet = (use @@ node) ++ ((!liveOut @@ node) -- (def @@ node)) in
    let newLiveOut : liveSet =  List.fold_left (fun out s ->
      out ++ (!liveIn @@ s)) Temp.Set.empty (Graph.succ node) in
    liveIn := Graph.Table.add node newLiveIn !liveIn;
    liveOut := Graph.Table.add node newLiveOut !liveOut;
    if oldLiveIn <> newLiveIn || oldLiveOut <> newLiveOut then terminate := false in
  let rec repeat() : unit =
    List.iter solve nodes;
    if !terminate then () else repeat() in
  let () = init(); repeat() in

  (* make interference graph *)
  let all_temps : Temp.temp list =
    Temp.Set.elements
      (List.fold_left
        (fun ts node -> ts ++ (!liveIn @@ node) ++ (!liveOut @@ node))
          Temp.Set.empty nodes) in
  let graph = Graph.newGraph() in
  let temp2node : (Temp.temp * Graph.node) list  =
    List.map (fun temp ->
      let node = Graph.newNode graph in (temp, node)) all_temps in
  let moves : (Graph.node * Graph.node) list ref = ref [] in
  let makeMoves fnode : unit =
    if Graph.Table.find fnode ismove
      then
        let [from] = Temp.Set.elements(use @@ fnode) in
        let [to'] = Temp.Set.elements(def @@ fnode) in
        moves := (List.assoc from temp2node, List.assoc to' temp2node) :: !moves in
  let buildGraph (temp, inode) =
    (* fnode: node of FlowGraph, inode: node of InterferenceGraph *)
    List.iter (fun fnode ->
      let itf_temps = Temp.Set.elements(!liveIn @@ fnode) in
      if List.mem temp itf_temps
        then
          List.iter (fun itf_t ->
            try
              if temp <> itf_t then
              Graph.mk_edge(inode, List.assoc itf_t temp2node)
            with
              Not_found -> ErrorMsg.impossible "at Liveness.interferenceGraph"
              ) itf_temps;
      makeMoves fnode) nodes;
      in
  List.iter buildGraph temp2node;
  let igraph = IGRAPH{ graph
                     ; tnode=(fun t -> List.assoc t temp2node)
                     ; gtemp=(fun n -> List.assoc n
                        (List.map (fun (x,y) -> (y,x)) temp2node))
                     ; moves=(!moves)} in
  let node2outs = fun n -> Temp.Set.elements(Graph.Table.find n !liveOut) in
  (igraph, node2outs)

let show _ = ()