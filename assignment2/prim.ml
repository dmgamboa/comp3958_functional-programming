module Edge = struct
    type t = Graph.edge

    let compare (u, v, w) (u', v', w') =
        if (u = u' && v = v') || (u = v' && v = u') then 0
        else Int.compare w w'
end

module Edges = Set.Make(Edge)

(* given a starting vertex u and a list of pairs l
    where each pair (v, w) corresponds to a path from u to v with weight w,
    returns a list of edges (u, v, w) *)
let pair_of_edge u l = List.map (fun (v, w) -> (u, v, w)) l

(* given a list of vertices l in graph g,
    returns a set of edges where for each edge (u, v, w),
    u is in l, and v is not in l *)
let connected_edges l g = l 
    |> List.fold_left (fun acc u ->
        Edges.union acc 
            (Graph.neighbours u g |> pair_of_edge u |> Edges.of_list)
    ) Edges.empty
    |> Edges.filter (fun (u, v, w) -> (List.mem u l) && not (List.mem v l))

(* finds the minimum spanning tree of a list using Prim's algorithm *)
let min_tree v0 g =
    if (g = Graph.empty) || not (Graph.is_vertex v0 g) then []
    else 
        let max_edges = (Graph.vertices g |> List.length) - 1 in
        let rec min_tree' l t =
            if (List.length t) = max_edges then List.rev t
            else
                let edges = connected_edges l g in
                let ((_, v, _) as e) = Edges.min_elt edges
                in min_tree' (v::l) (e::t)
        in min_tree' (v0::[]) []

