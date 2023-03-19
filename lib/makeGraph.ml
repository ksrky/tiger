let instr2graph (ilist : Assem.instr list) : Flow.flowgraph * Flow.Graph.node list =
  let initialFGraph =
    Flow.FGRAPH
      { control= Graph.newGraph ()
      ; def= Graph.Table.empty
      ; use= Graph.Table.empty
      ; ismove= Graph.Table.empty }
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
  let ordered_nodes = ref [] (* corresponding to instruction order *) in
  let makeGraph (Flow.FGRAPH {control; def; use; ismove}) instr =
    let node = Flow.Graph.newNode control in
    ordered_nodes := node :: !ordered_nodes;
    match instr with
    | Assem.OPER {dst; src; jump; _} ->
        (match jump with None -> () | Some jmps -> List.iter (map_latter_labeled_edge node) jmps);
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
  let resultFGraph =
    (* note: making flow graph in reverse order *)
    List.fold_left makeGraph initialFGraph (List.rev ilist)
  in
  List.iteri
    (fun i node -> Graph.mk_edge (List.nth !ordered_nodes i, node))
    (List.tl !ordered_nodes);
  (resultFGraph, !ordered_nodes)

let show (out : out_channel) (graph : Flow.flowgraph) (nodes : Graph.node list) : unit =
  let f t = try Temp.Table.find t Frame.tempMap with Not_found -> Temp.makestring t in
  let (FGRAPH {def; use; _}) = graph in
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
