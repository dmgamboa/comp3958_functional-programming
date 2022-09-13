(* GAMBOA, Donna (A01207448) *)

(* Taken from lec1, thank you! :) *)
let reverse l =
  let rec reverse' acc l =
    match l with
    | [] -> acc
    | x::xs -> reverse' (x::acc) xs
  in reverse' [] l



(* 1. *)

(* a. val range : int -> int -> int list 
    Returns the list of integers between a & b inclusive *)
let range a b =
    let rec range' a b acc =
        if b < a then acc
        else range' a (b - 1) (b::acc)
    in range' a b []

(* b. val drop :  int -> 'a list -> 'a list
    Returns the list with first n elements dropped *)
let rec drop n lst =
    match lst with
    | [] -> []
    | _ when n <= 0 -> lst
    | _::xs -> drop (n - 1) xs

(* c. val unzip : ('a * 'b) list -> 'a list * 'b list
    Returns pair of lists where the first list contains the first element of each pair
    and the second list contains the second element of each pair *)
let unzip lst =
    let rec unzip' lst firsts lasts =
        match lst with
        | [] -> (reverse firsts, reverse lasts)
        | (first, last)::xs -> unzip' xs (first::firsts) (last::lasts)
    in unzip' lst [] []
            
(* d. val dedup : 'a list -> 'a list
    Returns a list where where all consecutive duplicated elements are collapsed *)
let dedup lst =
    let rec dedup' lst acc =
        match lst with
        | [] -> reverse acc
        | x::[] -> dedup' [] (x::acc)
        | x::y::rest -> 
            if x = y then dedup' (y::rest) acc
            else dedup' (y::rest) (x::acc)
    in dedup' lst []



(* 2. *)

(* val exp : float -> int -> float 
    Returns the sum of the first n terms of the exponential function 
    exp 1.0 20;; prints - : float = 2.71828182845904553 *)
let exp x n =
    let next x curr =
        if curr = 0 then 1.
        else x /. (float_of_int curr) in
    let rec exp' x n curr term acc =
        if curr = n then acc
        else exp' x n (curr + 1) (term *. (next x (curr + 1))) (acc +. term)
    in exp' x n 0 1. 0.