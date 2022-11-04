include Anneal

(* val read_distances : string -> float array array 
Takes a file name and reads the data in the file into a matrix *)
let read_distances f =
    let file = open_in f in
    let rec get_row acc line = 
        match line with
        | "" -> acc |> List.rev |> Array.of_list
        | _ -> Scanf.sscanf line " %d %[^\n]" 
            @@ fun x s -> get_row ((float_of_int x)::acc) s in
    let rec read_distances' rows matrix =
        try
            (get_row [] @@ input_line file)::matrix
            |> read_distances' (rows + 1)
        with
        | End_of_file -> matrix |> List.rev |> Array.of_list
    in read_distances' 0 []

let generate_starting_path n =
    let rec generate_starting_path n l =
        if (n > 0)
        then generate_starting_path (n - 1) (n::l)
        else l
    in generate_starting_path (n - 1) []

(* Generates a random number pair (x, y)
    where x != y, and x and y are in range [0, b) *)
let generate_random_pairs b =
    let rec generate_random_pairs' (x, y) b =
        if (x = y)
        then generate_random_pairs' (Random.int b, Random.int b) b
        else (x, y)
    in generate_random_pairs' (Random.int b, Random.int b) b

let rec swap l a b =
    match l with
    | [] -> []
    | (x::xs) when x = a -> b::(swap xs a b)
    | (x::xs) when x = b -> a::(swap xs a b)
    | (x::xs) -> x::(swap xs a b)

let next s =
    let (x, y) = generate_random_pairs @@ List.length s
    in swap s (List.nth s x) (List.nth s y)

let energy m s =
    let rec energy' v s acc =
        match s with
        | [] -> acc +. v.(0)
        | (x::xs) -> energy' m.(x) xs (acc +. v.(x))
    in energy' m.(0) s 0.

let run f (t0, tf, ti) iterations =
    let distances = read_distances f in
    let s0 = generate_starting_path (Array.length distances) in
    let energy' = energy distances in
    let min_path = Anneal.run (s0, energy', next) (t0, tf, ti) iterations
    in ((energy' min_path), min_path)
    