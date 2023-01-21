let instr2graph (ilist : Assem.instr list) : Flow.flowgraph * Flow.Graph.node list =
  let initialFGraph =
    Flow.FGRAPH
      { control= Graph.newGraph ()
      ; def= Graph.Table.empty
      ; use= Graph.Table.empty
      ; ismove= Graph.Table.empty }
  in
  let next : Graph.node option ref = ref None in
  let map_next_edge (from : Graph.node) =
    match !next with
    | None -> ()
    | Some to' ->
        Graph.mk_edge (from, to');
        next := Some from
  in
  let label2itsnode : (Temp.label * Graph.node) list ref = ref [] in
  let label2fromnode : (Temp.label * Graph.node) list ref = ref [] in
  let map_latter_labeled_edge (from : Graph.node) lab =
    match List.assoc_opt lab !label2itsnode with
    | None -> label2fromnode := (lab, from) :: !label2fromnode
    | Some to' -> Graph.mk_edge (from, to')
  in
  let map_former_labeled_edge lab (to' : Graph.node) =
    match List.assoc_opt lab !label2fromnode with
    | None -> ()
    | Some from -> Graph.mk_edge (from, to')
  in
  let makeGraph (Flow.FGRAPH {control; def; use; ismove}) instr =
    let node = Flow.Graph.newNode control in
    match instr with
    | Assem.OPER {dst; src; jump; _} ->
        ( match jump with
        | None -> map_next_edge node
        | Some jmps -> List.iter (map_latter_labeled_edge node) jmps );
        (* temp *)
        Flow.FGRAPH
          { control
          ; def= Graph.Table.add node (Temp.Set.of_list dst) def
          ; use= Graph.Table.add node (Temp.Set.of_list src) use
          ; ismove= Graph.Table.add node false ismove }
    | Assem.LABEL {lab; _} ->
        label2itsnode := (lab, node) :: !label2itsnode;
        map_former_labeled_edge lab node;
        Flow.FGRAPH {control; def; use; ismove= Graph.Table.add node false ismove}
    | Assem.MOVE {dst; src; _} ->
        Flow.FGRAPH
          { control
          ; def= Graph.Table.add node (Temp.Set.of_list [dst]) def
          ; use= Graph.Table.add node (Temp.Set.of_list [src]) use
          ; ismove= Graph.Table.add node true ismove }
  in
  let (Flow.FGRAPH {control; _} as resultFGraph) =
    (* note: making flow graph in reverse order *)
    List.fold_left makeGraph initialFGraph (List.rev ilist)
  in
  (resultFGraph, Flow.Graph.nodes control)

let show (out : out_channel) (graph : Flow.flowgraph) : unit =
  let f t = try Temp.Table.find t Frame.tempMap with Not_found -> Temp.makestring t in
  let (FGRAPH {control; def; use; _}) = graph in
  let nodes = Graph.nodes control in
  output_string out "def\t\t\t\t\tuse\n";
  List.iter
    (fun node ->
      let defn = try Temp.Set.elements (Graph.Table.find node def) with Not_found -> [] in
      let usen = try Temp.Set.elements (Graph.Table.find node use) with Not_found -> [] in
      let s1 = String.concat ", " (List.map f defn) in
      output_string out (s1 ^ try String.make (40 - String.length s1) ' ' with _ -> "\t");
      let s2 = String.concat ", " (List.map f usen) in
      output_string out (s2 ^ "\n") )
    nodes
