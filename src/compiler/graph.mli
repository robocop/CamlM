type 'a graph

val add_node : 'a -> 'a graph -> 'a graph
val add_arc : 'a -> 'a -> 'a graph -> 'a graph
val adjacent_nodes : 'a graph -> 'a -> 'a list
val is_adjacent : 'a graph -> 'a -> 'a -> bool
val distance : 'a graph -> 'a -> 'a -> int
val node_present : 'a -> 'a graph -> bool

val empty : 'a graph
