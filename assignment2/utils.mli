(* returns the total of all weights in the list of edges *)
val sum_weights : Graph.edge list -> int

(* takes the name of a file and returns an abstract graph
    each line of the file contains the names of the two vertices 
    followed by an integer, delimited by space (' ') *)
val read_graph : string -> Graph.t