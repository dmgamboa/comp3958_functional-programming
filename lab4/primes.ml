(* GAMBOA, Donna (A01207448) *)

module PermutationMap = struct
    include Map.Make(String)

    let to_sorted_string s =
        List.init (String.length s) (String. get s)
        |> List.sort Char.compare
        |> List.to_seq
        |> String.of_seq
    
    let rec map_by_permutation m l =
        match l with
        | [] -> m
        | (x::xs) ->
            let key = to_sorted_string x in
            try
                let num_perms = find key m in 
                    map_by_permutation (m |> add key (num_perms + 1)) xs
            with
            | Not_found -> map_by_permutation (m |> add key 1) xs

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
    |> Printf.printf "The size of the largest set of permutations is %d\n"