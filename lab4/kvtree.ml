(* GAMBOA, Donna (A01207448) *)

module type OrderedType = sig
    type t
    val compare : t -> t -> int
end

module type S = sig
  type key
  type 'a t = Leaf | Node of key * 'a * 'a t * 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val insert : 'a t -> key -> 'a -> 'a t
  val find : 'a t -> key -> 'a option
  val delete : 'a t -> key -> 'a t
  val of_list : (key * 'a) list -> 'a t
  val to_list: 'a t -> (key * 'a) list
end

module Make(Ord : OrderedType) = struct
  type key = Ord.t
  type 'a t = Leaf | Node of key * 'a * 'a t * 'a t

  let empty = Leaf

  let is_empty t = t = Leaf

  let rec insert t k v =
    match t with
    | Leaf -> Node (k, v, Leaf, Leaf)
    | Node (k', v', l, r) when Ord.compare k k' < 0 ->
      Node (k', v', insert l k v, r)
    | Node (k', v', l, r) when Ord.compare k k' > 0 ->
      Node (k', v', l, insert r k v)
    | _ -> t

  let rec find t k =
    match t with
    | Leaf -> None
    | Node (k', _, l, r) when Ord.compare k k' < 0 ->
      find l k
    | Node (k', _, l, r) when Ord.compare k k' > 0 ->
      find r k
    | Node (_, v, _, _) -> Some v

  let rec rightmost t =
    match t with
    | Leaf -> failwith "rightmost: empty tree"
    | Node (k, v, _, Leaf) -> (k, v)
    | Node (_, _, _, r) -> rightmost r

  let rec delete t k =
    match t with
    | Leaf -> Leaf
    | Node (k', v, l, r) when Ord.compare k k' < 0 ->
      Node (k', v, delete l k, r)
    | Node (k', v, l, r) when Ord.compare k k' > 0 ->
      Node (k', v, l, delete r k)
    | Node (_, _, l, Leaf) -> l
    | Node (_, _, Leaf, r) -> r
    | Node (_, _, l, r) ->
      let (k', v') = rightmost l in
      Node (k', v', delete l k', r)

  let of_list l =
    List.fold_left (fun t (k, v) -> insert t k v) Leaf l

  let to_list t = 
    let rec to_list' t acc =
      match t with
      | Leaf -> acc
      | Node (k, v, Leaf, Leaf) -> (k, v)::acc
      | Node (k, v, Leaf, r) -> (k, v)::(to_list' r acc)
      | Node (k, v, l, Leaf) -> (k, v)::(to_list' l acc)
      | Node (k, v, l, r) -> (k,v)::(to_list' l (to_list' r acc))
    in to_list' t []
end
