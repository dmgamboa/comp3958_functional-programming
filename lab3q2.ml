(* GAMBOA, Donna (A01207448) *)

(* Binary search tree type where each value is a key-value pair **)

type ('a, 'b) bstree = Leaf | Node of 'a * 'b * ('a, 'b) bstree * ('a, 'b) bstree

(* val bstree_insert : ('a, 'b) bstree -> 'a -> 'b -> ('a, 'b) bstree
	Returns a bstree with a (key, value) pair inserted **)
let rec bstree_insert t key value =
	match t with
	| Leaf -> Node (key, value, Leaf, Leaf)
	| Node (key', value', l, r) when key < key' 
		-> Node (key', value', bstree_insert l key value, r)
	| Node (key', value', l, r) when key > key' 
		-> Node (key', value', l, bstree_insert r key value)
	| Node (_, _, l, r) -> Node (key, value, l, r)

(* val bstree_of_list : ('a * 'b) list -> ('a, 'b) bstree
	Returns a bstree from a list of tuples **)
let bstree_of_list l =
	List.fold_left (fun t (key, value) -> bstree_insert t key value) Leaf l

(* val bstree_size : ('a, 'b) bstree -> int
	Returns the number of nodes in the bstree **)
let rec bstree_size t =
	match t with
	| Leaf -> 0
	| Node (_, _, l, r) -> 1 + bstree_size l + bstree_size r

(* val bstree_height : ('a, 'b) bstree -> int
 	Returns the height of the bstree **)
let rec bstree_height t =
	match t with
	| Leaf -> 0
	| Node (_, _, l, r) -> 1 + max (bstree_height l) (bstree_height r)

(* val bstree_find : ('a, 'b) bstree -> 'a -> 'b option
	Returns Some value of the key arg passed;
	Returns None if the key is not in the bstree **)
let rec bstree_find t key =
	match t with
	| Leaf -> None
	| Node (key', _, l, r) when key < key' -> bstree_find l key
	| Node (key', _, l, r) when key > key' -> bstree_find r key
	| Node (_, value, l, r ) -> Some value

(* val bstree_max : ('a, 'b) bstree -> 'a * 'b
	Returns the (key, value) tuple of the maximum value of key in the bstree**)
let rec bstree_max t =
	match t with
	| Leaf -> failwith "bstree_max: empty tree has no maximum"
	| Node (key, value, _, Leaf) -> (key, value)
	| Node (_, _, _, r) -> bstree_max r

(* val bstree_delete : ('a, 'b) bstree -> 'a -> ('a, 'b) bstree
	Returns the bstree with the Node of the key arg removed **)
let rec bstree_delete t key =
	match t with
	| Leaf -> Leaf
	| Node (key', value, l, r) when key < key'
		-> Node (key', value, bstree_delete l key, r)
	| Node (key', value, l, r) when key > key'
		-> Node (key', value, l, bstree_delete r key)
	| Node (_, _, Leaf, r) -> r
	| Node (_, _, l, Leaf) -> l
	| Node (_, _, l, r) ->
		let (max_k, max_v) = bstree_max l in
		Node (max_k, max_v, bstree_delete l max_k, r)
