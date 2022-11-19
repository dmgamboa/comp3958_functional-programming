(* GAMBOA, Donna A01207448 *)

type 'a infstream = Cons of 'a * (unit -> 'a infstream)

let rec from n = Cons (n, fun () -> from (n+1))

let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h::take (n-1) (t ())

(* val filter : ('a -> bool) -> 'a infstream -> 'a infstream
  Returns an infstream with values filtered out based on f *)
let rec filter f (Cons (h, t)) =
  if f h then Cons (h, fun () -> filter f (t ()))
  else filter f (t ())

(* val primes : int infstream = Cons (2, <fun>)
  Returns an infinite stream of prime numbers *)
let primes = 
  let remove_multiples n l = filter (fun x -> x mod n <> 0) l in
  let rec primes' (Cons (h, t)) =
    Cons (h, fun () -> primes' @@ remove_multiples h @@ t ())
  in primes' (from 2)

(* Answer to 1b *)
let first_100_primes = take 100 primes