(* GAMBOA, Donna (A01207448) *)



(* 1. *)

(* a. val min_elt : 'a list -> a
    Returns the smallest element in a non-empty-list *)
let min_elt l = List.fold_left
    (fun acc x -> if acc < x then acc else x) (List.hd l) l

(* b. val remove : 'a -> 'a list -> 'a list
    Returns a list l with the first occurrence of x removed *)
let rec remove x = function
    | [] -> []
    | y::ys when x <> y -> y::(remove x ys)
    | _::ys -> ys

(* c. val selection_sort : 'a list -> 'a list
    Returns sorted list by selecting the smallest element of the list
    and moving it to the front *)
let rec selection_sort = function
    | [] -> []
    | x::xs as l -> let m = min_elt(l) in
        m::(selection_sort @@ remove m l)



(* 2. *)

(* a. val group : 'a list -> 'a list list 
    Returns a list containing list of consecutive elements that are the same *)
let group l = 
    List.fold_right (fun curr acc -> 
        match acc with
        | [] -> [[curr]]
        | (x::xs) ->
            match x with
            | y::ys as l when y = curr -> (curr::l)::xs
            | _ -> [curr]::acc
    ) l []

(* b. val frequencies : 'a list -> ('a * int) list 
    Returns list of tuples where first element in the tuple is the element found in the list
    and the second element is the number of times it appears in the list *)
let frequencies l =
    let sorted_list = List.sort (fun x y -> 
        if x < y then 1 else (-1)
    ) l
    in List.fold_left (fun acc curr ->
        match acc with
        | [] -> [(curr, 1)]
        | (value, freq)::xs as l ->
            if curr = value then (value, (freq + 1))::xs
            else (curr, 1)::l
    ) [] sorted_list