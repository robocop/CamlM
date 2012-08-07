(** A simple and incomplete directed graph implementation.
   
    Also rather inefficient.
  *)

(** A graph. Currently implemented as a list of (node, edges). *)
type 'a graph

(** Add a node with no edges to a graph. *)
val add_node : 'a -> 'a graph -> 'a graph

(** Add a directed edge from a node to another in the graph.
   
    @raise Not_found if the source node was not found.
  *)
val add_arc : 'a -> 'a -> 'a graph -> 'a graph

(** Returns a list of the nodes which have an edge from the source node to them.
   
    @param node the source node.
  *)
val adjacent_nodes : 'a graph -> 'a -> 'a list

(** Checks that a has an edge to b. *)
val is_adjacent : 'a graph -> 'a -> 'a -> bool

(** Cheap distance in between two node. Used for module scope.
  
   - 0 <= distance <= 1, then the dst module is in scope.
   - distance > 1, then the dst module is not exposed to src.
 *)
val distance : 'a graph -> 'a -> 'a -> int

(** Checks node is present in graph. *)
val node_present : 'a -> 'a graph -> bool

(** The empty graph. Combine with add_node and add_arc to build a graph. *)
val empty : 'a graph
