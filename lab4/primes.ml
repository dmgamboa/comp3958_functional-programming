(* GAMBOA, Donna (A01207448) *)

module PermutationMap = struct
    include Map.Make(String)

    (* val to_sorted_string : key -> key
    Returns a string with each character sorted based on its ASCII value *)
    let to_sorted_string s =
        List.init (String.length s) (String. get s)
        |> List.sort Char.compare
        |> List.to_seq
        |> String.of_seq
    
    (* val map_by_permutation :  int t -> key list -> int t
    Given a list of strings, returns a map with key-value pairs corresponding 
    to the sets of permutations within that list *)
    let map_by_permutation m l = 
        let num_perms key m = 
        try
            (find key m) + 1
        with
        | Not_found -> 1
        in List.fold_left (fun acc curr -> 
            let key = to_sorted_string curr in
            acc |> add key @@ num_perms key acc
        ) m l
    
    (* val get_max_perms : int t -> int 
    Returns the key with the highest stored int value *)
    let get_max_perms m = fold (fun k v acc -> if v > acc then v else acc) m 0
end

(* val get_input : unit -> string list
    Returns list of strings read from the input stream *)
let get_input () =
    let rec get_input' acc =
        match acc with
        | x::xs when x = "" -> List.rev xs
        | _ -> get_input' @@ (Scanf.scanf " %s"  @@ fun x -> x)::acc
    in get_input' []

let () = 
    get_input ()
    |> PermutationMap.map_by_permutation PermutationMap.empty
    |> PermutationMap.get_max_perms
    |> Printf.printf "The largest set of permutations has size %d\n"