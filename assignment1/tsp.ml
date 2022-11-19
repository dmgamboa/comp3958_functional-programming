(* val read_distances : string -> float array array 
    Takes a file name and reads the data in the file into a matrix *)
let read_distances f =
    let file = open_in f in
    let rec get_row acc line = 
        match line with
        | "" -> acc |> List.rev |> Array.of_list
        | _ -> Scanf.sscanf line " %s %[^\n]" 
            @@ fun x s -> get_row ((float_of_string x)::acc) s in
    let rec read_distances' rows matrix =
        try
            (get_row [] @@ input_line file)::matrix
            |> read_distances' (rows + 1)
        with
        | End_of_file -> close_in file ;
            matrix |> List.rev |> Array.of_list
    in read_distances' 0 []

(* val generate_random_pairs : int -> int * int
    Generates an int tuple (x, y)
    where x != y, and x and y are in range [0, b) *)
let generate_random_pairs b =
    let rec generate_random_pairs' (x, y) b =
        if (x = y)
            then generate_random_pairs' (Random.int b, Random.int b) b
            else (x, y)
    in generate_random_pairs' (Random.int b, Random.int b) b

(* val swap : 'a list -> 'a -> 'a -> 'a list 
    Returns a list l with values for a and b swapped *)
let rec swap l a b =
    match l with
    | [] -> []
    | (x::xs) when x = a -> b::(swap xs a b)
    | (x::xs) when x = b -> a::(swap xs a b)
    | (x::xs) -> x::(swap xs a b)

(* val next : 'a list -> 'a list 
    Returns a list with two of randomly chosen elements swapped '*)
let next s =
    let (x, y) = generate_random_pairs @@ List.length s
    in swap s (List.nth s x) (List.nth s y)

(* val energy : float array array -> int list -> float
    Assuming the path starts and ends at vertex 0,
    Returns the cost of traversing matrix m 
    given itinerary of vertices s *)
let energy m s =
    let rec energy' v s acc =
        match s with
        | [] -> acc +. v.(0)
        | (x::xs) -> energy' m.(x) xs (acc +. v.(x))
    in energy' m.(0) s 0.

(* val run : string -> float * float * int -> int -> float * int list
    Runs simulated annealing on a matrix taken from filename f
    Returns a tuple containing the distance of the approximate shortest path
    and its associated itinerary *)
let run f (t0, tf, ti) iterations =
    let distances = read_distances f in
    let s0 = List.init (Array.length distances - 1) (fun x -> x + 1) in
    let energy' = energy distances in
    let min_path = Anneal.run (s0, energy', next) (t0, tf, ti) iterations
    in ((energy' min_path), min_path)