(* given a starting vertex u and a list of pairs l
    where each pair (v, w) corresponds to a path from u to v with weight w,
    returns a list of edges (u, v, w) *)
let pair_of_edge u l = List.map (fun (v, w) -> (u, v, w)) l

(* given a list of vertices s in graph g,
    returns a list of edges where for each edge (u, v, w),
    u is in s, and v is not in s *)
let connected_edges l g = l 
    |> List.fold_left (fun acc u -> 
        (Graph.neighbours u g |> pair_of_edge u) @ acc) []
    |> List.filter (fun (u, v, w) -> (List.mem u l) && not (List.mem v l))

(* given a list of edges, returns an edge with the minimum weight *)
let min l = List.fold_left (fun ((_, _, w) as e) ((_, _, w') as e') -> 
    if w < w' then e else e'
) ("", "", Graph.inf) l

(* finds the minimum spanning tree of a list using Prim's algorithm *)
let min_tree v0 g =
    if (g = Graph.empty) || not (Graph.is_vertex v0 g) then []
    else 
        let max_edges = (Graph.vertices g |> List.length) in
        let rec min_tree' l t =
            if (List.length l) = max_edges then t
            else
                let edges = connected_edges l g in
                let ((_, v, _) as e) = min edges
                in min_tree' (v::l) (e::t)
        in min_tree' (v0::[]) []

