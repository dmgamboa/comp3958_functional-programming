(* val run : 'a * ('a -> float) * ('a -> 'a)  -> float * float * int -> int -> 'a
	Runs simulated annealing *)
let run (s, energy, next) (t, factor, interval) num_steps =
	let int_start = num_steps mod interval + 1 in
	let get_prob e e' t =
		if (e < e')
			then exp ((e -. e') /. t)
			else 1. in
	let get_temp t num_steps =
		if num_steps mod interval = int_start
			then (t *.factor)
			else t in
	let rec anneal s t num_steps =
		match num_steps with
		| 0 -> s
		| _ ->
			let t' = get_temp t num_steps in
			let s' = next s in
			let p = get_prob (energy s) (energy s') t' in
			let next_state = 
				if p >= Random.float 1.
					then s'
					else s
			in anneal next_state t' (num_steps - 1)
	in anneal s t num_steps