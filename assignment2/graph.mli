type t                             (* the abstract graph type *)
type edge = string * string * int  (* the edge type *) 

exception Except of string  (* exception raised by some of the functions *)

val inf : int    (* special large value returned by weight function *)

(* the empty graph *)
val empty : t

(* adds an edge to a graph
   * raises an exception when a conflicting edge is being added e.g. 
     empty |> add_edge ("A", "B", 1) |> add_edge ("B", "A", 2)
     or when an edge joining a vertex to itself is being added
   * a no-op when the "same" edge is added a second time
*)
val add_edge : edge -> t -> t

(* returns a graph from the specified list of edges; may raise an exception *)
val of_list : edge list -> t

(* returns the list of all vertices in a graph *)
val vertices : t -> string list

(* returns whether a string is the name of a vertex in a graph *)
val is_vertex : string -> t -> bool

(* returns a list of all edges in a graph *)
val edges : t -> edge list 

(* returns a list of all neighbours of a specified vertex in a graph;
   each pair in the list represents (neighbour_vertex, weight);
   returns the empty list if the vertex is not in the graph *)
val neighbours : string -> t -> (string * int) list

(* [weight v1 v2 g] returns the weight of an edge between vertices v1 & v2 
   in graph g; it assumes that there is at most 1 edge connecting 2 vertices;
   it returns the special value inf if there is no edge between v1 & v2 *)
val weight : string -> string -> t -> int 
