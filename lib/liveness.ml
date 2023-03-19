type igraph =
  | IGRAPH of
      { graph: Graph.graph
      ; tnode: Temp.temp -> Graph.node
      ; gtemp: Graph.node -> Temp.temp
      ; moves: (Graph.node * Graph.node) list }

type liveSet = Temp.Set.t

type liveMap = liveSet Graph.Table.t

let ( @@ ) (gt : liveMap) (n : Graph.node) : liveSet =
  try Graph.Table.find n gt
  with Not_found -> Temp.Set.empty (* a node of Assem.Label instruction doesn't have dst or use *)

let ( ++ ) : liveSet -> liveSet -> liveSet = Temp.Set.union

let ( -- ) : liveSet -> liveSet -> liveSet = Temp.Set.diff

let interferenceGraph (Flow.FGRAPH {control; def; use; ismove} : Flow.flowgraph) :
    igraph * (Flow.Graph.node -> Temp.temp list) =
  (* Calculate liveness information *)
  let nodes : Flow.Graph.node list = Flow.Graph.nodes control in
  let initLiveInOut =
    List.fold_left
      (fun (liveIn, liveOut) node ->
        (Graph.Table.add node Temp.Set.empty liveIn, Graph.Table.add node Temp.Set.empty liveOut) )
      (Graph.Table.empty, Graph.Table.empty)
      nodes
  in
  let changed : bool ref = ref false in
  let solveLiveness (liveIn, liveOut) (node : Flow.Graph.node) : liveMap * liveMap =
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
    let newLiveInOut = List.fold_left solveLiveness liveInOut nodes in
    if !changed then (
      changed := false;
      repeat newLiveInOut )
    else newLiveInOut
  in
  let liveIn, liveOut = repeat initLiveInOut in
  let _debugLiveInOut (liveIn, liveOut) =
    print_endline "out\t\tin";
    List.iter
      (fun node ->
        let li = Temp.Set.elements (liveIn @@ node) in
        let lo = Temp.Set.elements (liveOut @@ node) in
        print_string (String.concat "," (List.map string_of_int lo));
        print_string "\t\t";
        print_endline (String.concat "," (List.map string_of_int li)) )
      nodes
  in
  let () = _debugLiveInOut (liveIn, liveOut) in 
  (* Make interference graph *)
  let all_temps : Temp.temp list =
    Temp.Set.elements
      (List.fold_left (fun ts node -> ts ++ (use @@ node) ++ (def @@ node)) Temp.Set.empty nodes)
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
  let makeMoves fnode : (Temp.temp * Temp.temp) option =
    if Graph.Table.find fnode ismove then (
      let [from] = Temp.Set.elements (use @@ fnode) in
      let [to'] = Temp.Set.elements (def @@ fnode) in
      ( try moves := (List.assoc from temp2node, List.assoc to' temp2node) :: !moves
        with Not_found -> ErrorMsg.impossible "Tiger.Liveness.interferenceGraph" );
      Some (from, to') )
    else None
  in
  let buildGraph () =
    List.iter
      (fun fnode ->
        let mv_opt = makeMoves fnode in
        let itf_temps = Temp.Set.elements ((liveIn @@ fnode) ++ (liveOut @@ fnode)) in
        List.iter
          (fun u ->
            let ts =
              match mv_opt with
              | Some (from, to') when from = u -> List.filter (( <> ) to') itf_temps
              | _ -> itf_temps
            in
            List.iter
              (fun v ->
                if u <> v then
                  try
                    Graph.mk_edge (List.assoc u temp2node, List.assoc v temp2node);
                    Graph.mk_edge (List.assoc v temp2node, List.assoc u temp2node)
                  with Not_found -> ErrorMsg.impossible "at Liveness.interferenceGraph" )
              ts )
          itf_temps )
      nodes
  in
  buildGraph ();
  let igraph =
    IGRAPH
      { graph
      ; tnode= (fun t -> List.assoc t temp2node)
      ; gtemp= (fun n -> List.assoc n (List.map (fun (x, y) -> (y, x)) temp2node))
      ; moves= !moves }
  in
  let node2outs n = Temp.Set.elements (Graph.Table.find n liveOut) in
  (igraph, node2outs)

let show (out : out_channel) (IGRAPH {graph; gtemp; moves; _} : igraph) : unit =
  let prnode n =
    try Temp.Table.find (gtemp n) Frame.tempMap with Not_found -> Temp.makestring (gtemp n)
  in
  let nodes = Graph.nodes graph in
  let matrix : char Array.t Array.t =
    Array.init (List.length nodes) (fun _ -> Array.init (List.length nodes) (fun _ -> ' '))
  in
  List.iteri
    (fun i u ->
      let inodes = Graph.adj u in
      let mv = List.assoc_opt u moves in
      List.iteri
        (fun j v ->
          if List.mem v inodes then matrix.(i).(j) <- 'o';
          if Some v = mv then matrix.(i).(j) <- 'm' )
        nodes )
    nodes;
  output_string out ("\t" ^ String.concat "\t" (List.map prnode nodes) ^ "\n");
  List.iteri
    (fun i row ->
      output_string out (prnode (List.nth nodes i) ^ "\t");
      List.iter (fun c -> output_string out ("   " ^ Char.escaped c ^ "\t")) (Array.to_list row);
      output_string out "\n" )
    (Array.to_list matrix)
