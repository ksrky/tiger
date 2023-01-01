type node' = int
type temp = Temp.temp

type noderep = NODE of {succ: node' list; pred: node' list}

let emptyNode = NODE{succ=[]; pred=[]}

let bogusNode = NODE{succ=[-1]; pred=[]}

let isBogus = function
  | NODE{succ=(-1::_); _} -> true
  | _ -> false

type graph = noderep array

type node = graph * node'
let eq (((_, a), (_, b)) : node * node) : bool = a = b

let augment (g: graph) (n: node') : node = (g, n)

let sizeAint = 128
let newGraph() = Array.make sizeAint bogusNode

let nodes g =
  let rec f i =
    if isBogus(g.(i))
      then []
      else (g, i) :: f (i+1) in
  f 0

let succ ((g, i) : node) : node list =
  let NODE{succ=s; _} = g.(i) in List.map (augment g) s

let pred ((g, i) : node) : node list =
  let NODE{pred=p; _} = g.(i) in List.map (augment g) p

let adj (gi : node) : node list = pred gi @ succ gi

let newNode (g : graph) =
  let rec look(lo, hi) =
  (* i < lo indicates i in use
     i >= hi indicates i not in use *)
    if lo = hi
      then (Array.set g lo emptyNode; (g, lo))
      else
        let m = (lo + hi) / 2 in
        if isBogus(g.(m)) then look(lo, m) else look(m+1, hi)
in look(0, Array.length g)

exception GraphEdge
let check((_g, _g') : graph * graph) = (* if g=g' then () else raise GraphEdge *) ()

let rec delete (i: node') ((j::rest): node' list) : node' list = if i = j then rest else j :: delete i rest

let diddle_edge (change : node' -> node' list -> node' list) (((g, i) : node), ((g', j) : node)) : unit =
  let () = check(g, g') in
  let NODE{succ=si; pred=pi} = g.(i) in
  let () = Array.set g i (NODE{succ=change j si; pred=pi}) in
  let NODE{succ=sj; pred=pj} = g.(j) in
  let () = Array.set g j (NODE{succ=sj; pred=change i pj}) in
  ()

let mk_edge : node * node -> unit = diddle_edge List.cons

let rm_edge : node * node -> unit = diddle_edge delete

module Table = Map.Make(struct
  type t = node
  let compare = Stdlib.compare
end)

type 'a table = 'a Table.t

let nodename(_, i) = "n" ^ string_of_int i