module Edge = struct
    type t = (string * string * int)

    let compare (u, v, w) (u', v', w') =
        if (u = u' && v = v') || (u = v' && v = u') then 0
        else Int.compare w w'
end

module Edges = Set.Make(Edge)

Edges.empty |> Edges.add ("A", "B", 1) |> Edges.add ("B", "A", 2)

Edges.fold (fun x y -> x::acc) [] 