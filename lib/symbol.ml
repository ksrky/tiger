type symbol = string * int

let nextsym = ref (-1)
let sizeHint = 128
let hashtable = Hashtbl.create ~random:true sizeHint
let symbol (name : string) : symbol = match Hashtbl.find_opt hashtable name with
  | Some i -> (name, i)
  | None ->
    incr nextsym;
    Hashtbl.add hashtable name !nextsym;
    name, !nextsym

let name (s, _) = s

module Table = Map.Make (struct
    type t = symbol
    let compare = Stdlib.compare
  end)

type 'a table = 'a Table.t
let empty = Table.empty
let enter (env, k, v) = Table.add k v env
let look (env, k) = Table.find_opt k env
let init l  = Table.of_seq (List.to_seq l)