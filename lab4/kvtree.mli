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
  val insert : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a option
  val delete : key -> 'a t -> 'a t
  val of_list : (key * 'a) list -> 'a t
  val to_list: 'a t -> (key * 'a) list
end

module Make(Ord : OrderedType) : S with type key = Ord.t
