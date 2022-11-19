module Graph = Map.Make(String)

type t = (string * int) list Graph.t                
type edge = string * string * int

exception Except of string  (* exception raised by some of the functions *)

let inf = max_int    (* special large value returned by weight function *)

(* the empty graph *)
let empty = Graph.empty

(* returns the list of all vertices in a graph *)
let vertices g = Graph.fold (fun k v acc -> k::acc) g []

(* returns whether a string is the name of a vertex in a graph *)
let is_vertex v g = List.mem v @@ vertices g

(* returns a list of all edges in a graph *)
let edges g = 
    let edges' u l = List.fold_left (fun acc (v, w) -> (u, v, w)::acc) [] l 
    in Graph.fold (fun k v acc -> (edges' k v) @ acc) g []

(* returns whether an edge is in a graph *)
let does_edge_exist e g = List.mem e @@ edges g

(* returns a list of all neighbours of a specified vertex in a graph;
   each pair in the list represents (neighbour_vertex, weight);
   returns the empty list if the vertex is not in the graph *)
let neighbours v g =
    match (Graph.find_opt v g) with
    | None -> []
    | Some l -> l

(* [weight v1 v2 g] returns the weight of an edge between vertices v1 & v2 
   in graph g; it assumes that there is at most 1 edge connecting 2 vertices;
   it returns the special value inf if there is no edge between v1 & v2 *)
let weight u v g =
    let rec weight' = function
    | [] -> inf
    | (v, w)::xs when u = v -> w
    | _::xs -> weight' xs
in weight' (neighbours v g)

(* returns whether the vertex is in a list of edges *)
let has_conflicting_edge v l = List.exists (fun (v',_) -> v = v') l

(* adds an edge to a graph
   * raises an exception when a conflicting edge is being added e.g. 
     empty |> add_edge ("A", "B", 1) |> add_edge ("B", "A", 2)
     or when an edge joining a vertex to itself is being added
   * a no-op when the "same" edge is added a second time
*)
let add_edge (u, v, w) g =
    if u = v then raise @@ Except ("Cannot join vertex to itself")
    else if (does_edge_exist (u, v, w) g) then g
    else match ((neighbours u g), (neighbours v g)) with
    | ([], []) 
        -> Graph.add u ((v, w)::[]) @@ Graph.add v ((u, w)::[]) g
    | (l, []) when not (has_conflicting_edge v l)
        -> Graph.add u ((v, w)::l) @@ Graph.add v ((u, w)::[]) g
    | ([], l) when not (has_conflicting_edge u l)
        -> Graph.add u ((v, w)::[]) @@ Graph.add v ((u, w)::l) g
    | (ul, vl) when not (has_conflicting_edge v ul)
        -> Graph.add u ((v, w)::ul) @@ Graph.add v ((u, w)::vl) g
    | _ -> raise @@ Except ("Conflicting edge found")

(* returns a graph from the specified list of edges; may raise an exception *)
let of_list l = List.fold_left (fun acc (u, v, w) -> 
    add_edge (u, v, w) acc
) empty l