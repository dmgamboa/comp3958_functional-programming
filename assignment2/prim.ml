module Edge = struct
    type t = Graph.edge

    let compare (u, v, w) (u', v', w') =
        if (u = u' && v = v') || (u = v' && v = u') then 0
        else Int.compare w w'
end

module Edges = Set.Make(Edge)

(* given a list of vertices l in graph g,
    returns a set of edges where for each edge (u, v, w),
    u is in l, and v is not in l *)
let connected_edges l g = l 
    |> List.fold_left (fun acc u ->
        Graph.neighbours u g 
        |> List.map (fun (v, w) -> (u, v, w))
        |> Edges.of_list
        |> Edges.union acc) Edges.empty
    |> Edges.filter (fun (u, v, w) -> (List.mem u l) && not (List.mem v l))

(* finds the minimum spanning tree of a list using Prim's algorithm *)
let min_tree v0 g =
    if (g = Graph.empty) || not (Graph.is_vertex v0 g) then []
    else 
        let rec min_tree' l t =
            let edges = connected_edges l g in
            if (edges = Edges.empty) then List.rev t
            else
                let ((_, v, _) as e) = Edges.min_elt edges
                in min_tree' (v::l) (e::t)
        in min_tree' [v0] []