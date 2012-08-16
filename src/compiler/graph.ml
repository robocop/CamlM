(** A simple and incomplete directed graph implementation.
   
    Also rather inefficient.
  *)

(** A graph. Currently implemented as a list of (node, edges). *)
type 'a graph = ('a * 'a list) list

(** Add a node with no edges to a graph. *)
let add_node node graph = 
  (node, []) :: graph

(** Add a directed edge from a node to another in the graph.
   
    @raise Not_found if the source node was not found.
  *)
let rec add_arc node arc = function
  | [] -> raise Not_found
  | (node', arcs) :: xs when node' = node -> (node', arc :: arcs) :: xs
  | x :: xs -> x :: add_arc node arc xs

(** Returns a list of the nodes which have an edge from the source node to them.
   
    @param node the source node.
  *)
let adjacent_nodes graph node = 
  try List.assoc node graph
  with Not_found -> []

(** Checks that a has an edge to b. *)
let is_adjacent graph a b = List.mem b (adjacent_nodes graph a)

(** Cheap distance in between two node. Used for module scoping.
  
   - 0 <= distance <= 1, then the dst module is in scope.
   - distance > 1, then the dst module is not exposed to src.
 *)
let distance graph src dst = match src, dst with
  | a, b when a = b -> 0
  | a, b when is_adjacent graph a b -> 1
  | _ -> 2

(** Checks node is present in graph. *)
let node_present node = List.exists (fun (node', _) -> node' = node)

(** The empty graph. Combine with add_node and add_arc to build a graph. *)
let empty = []

