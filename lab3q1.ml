(* GAMBOA, Donna (A01207448) *)

(* a. val words : unit -> string list
    Returns list of words read from file **)
let words () =
    let rec words' acc = 
    try
        words' @@ (Scanf.scanf " %s" (fun x -> 
                if (x = "") then raise End_of_file else x)
            )::acc
    with
    | End_of_file -> List.rev acc
    in words' []

(* b. val sum_ints : string list - > int
    Returns sum of integers from a list of strings **)
let sum_ints input =
    let get_int str = 
    try 
        int_of_string str
    with
    | _ -> 0
    in List.fold_left (fun acc curr -> acc + (get_int curr)) 0 input

(* Prints the sum of ints from a given input *)
let () =
    Printf.printf "%d\n" @@ sum_ints @@ words ();