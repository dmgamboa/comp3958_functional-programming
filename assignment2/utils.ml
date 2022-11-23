(* returns the total of all weights in the list of edges *)
let sum_weights l = List.fold_left (fun acc (u, v, w) -> acc + w) 0 l

(* takes the name of a file and returns an abstract graph
    each line of the file contains the names of the two vertices 
    followed by an integer, delimited by space (' ') *)
let read_graph f =
    let file = open_in f in
    let rec read_graph' edges =
        try
            let line = input_line file in
            let edge = Scanf.sscanf line " %s %s %d %[^\n]" 
                @@ fun u v w s -> (u, v, w)
            in read_graph' (edge::edges)
        with
        | End_of_file -> close_in file ;
            edges |> List.rev |> Graph.of_list
    in read_graph' []