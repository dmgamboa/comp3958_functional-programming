let prob e e' t = if (e < e')
	then exp((e -. e') /. t)
	else 1.

(* val run : 'a * ('a -> float) * ('a -> 'a)  -> float * float * int -> int -> 'a *)
let run (s, energy, next) (t, factor, interval) num_steps =
	let int_start = (num_steps mod interval) in
		let rec anneal (s, energy, next) (t, factor, interval) num_steps =
		match num_steps with
		| num_steps when num_steps <= 0 -> s
		| _ ->
			let t' =
				if (num_steps mod interval) = int_start
				then (t *. factor) else t in
			let s' = next s in
			let accept_prob = (prob (energy s) (energy s') t') in
			let next_state = 
				if accept_prob >= Random.float 1.
				then s else s'
			in anneal (next_state, energy, next) (t', factor, interval) 
				(num_steps - 1)
	in anneal (s, energy, next) (t, factor, interval) num_steps