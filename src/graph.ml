type 'a graph = ('a * 'a list) list

let add_node node graph = 
  (node, []) :: graph

let rec add_arc node arc = function
  | [] -> raise Not_found
  | (node', arcs) :: xs when node' = node -> (node', arc :: arcs) :: xs
  | x :: xs -> x :: add_arc node arc xs

let adjacent_nodes graph node = 
  try List.assoc node graph
  with Not_found -> []

let adjancent_nodes (adj, _) = adj

let is_adjacent graph a b = List.mem b (adjacent_nodes graph a)

(* If 0 <= distance <= 1, then the dst module is in scope.
 * If distance > 1, then the dst module is a dependency of one of the modules
 * imported by src.
 *)
let distance graph src dst = match src, dst with
  | a, b when a = b -> 0
  | a, b when is_adjacent graph a b -> 1
  | _ -> 2

let node_present node = List.exists (fun (node', _) -> node' = node)

let empty = []
