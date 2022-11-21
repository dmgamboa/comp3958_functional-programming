(* returns the total of all weights in the list of edges *)
let sum_weights l = List.fold_left (fun acc (u, v, w) -> acc + w) 0 l

(* takes the name of a file and returns an abstract graph
    each line of the file contains the names of the two vertices 
    followed by an integer, delimited by space (' ') *)
let read_graph f =
    let file = open_in f in
    let rec get_row acc line = 
        match line with
        | "" -> acc |> List.rev
        | _ -> Scanf.sscanf line " %s %s %d %[^\n]" 
            @@ fun u v w s -> get_row ((u, v, w)::acc) s in
    let rec read_graph' edges =
        try
            (get_row edges @@ input_line file) |> read_graph'
        with
        | End_of_file -> close_in file ;
            edges |> List.rev |> Graph.of_list
    in read_graph' []