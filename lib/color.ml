type allocation = Frame.register Temp.Table.t

type input = { interference: Liveness.igraph
             ; initial: allocation
             ; spillCost : Graph.node -> int
             ; registers: Frame.register list}

let color (_ :input) : allocation * Temp.temp list = raise ErrorMsg.Error

(*
let n_colors = 8 (* K *)

let precolored : Graph.Set.t = Graph.Set.empty
let simplifyWorklist : Graph.node list ref = ref []
let freezeWorklist : Graph.node list ref = ref []
let spillWorklist : Graph.node list ref = ref []
let spilledNodes : Graph.Set.t ref = ref Graph.Set.empty
let coalescedNodes : Graph.node list ref = ref []
let coloredNodes : Graph.Set.t ref = ref Graph.Set.empty
let selectStack : Temp.temp Stack.t = Stack.create()

let coalescedMoves : (Graph.node * Graph.node) list ref = ref []

let adjList : Graph.node list Temp.Table.t ref = ref Temp.Table.empty
let degree : (Graph.node * int) list ref  = ref []
let moveList : Temp.temp list Graph.Table.t = Graph.Table.empty
let alias v = List.assoc v !coalescedMoves
let color : int Graph.Table.t ref = ref Graph.Table.empty

let (++) : Graph.Set.t -> Graph.Set.t -> Graph.Set.t =Graph.Set.union
let (--) : Graph.Set.t -> Graph.Set.t -> Graph.Set.t = Graph.Set.diff

let rec getAlias n =
  if List.mem n !coalescedNodes
    then getAlias(alias n)
  else n  

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