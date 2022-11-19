(* GAMBOA, Donna A01207448 *)

type 'a lazystream = Cons of 'a * 'a lazystream Lazy.t

let rec from n = Cons (n, lazy (from (n + 1)))

let tl (Cons (_, t)) = Lazy.force t

let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h::take (n - 1) (Lazy.force t)

let rec map f (Cons (h, t)) =
  Cons (f h, lazy (map f (Lazy.force t)))

(* val exp_terms : float -> float lazystream 
  Returns an infinite stream consisting of the terms in the exponential series*)
let exp_terms x =
  let from_float n = map (fun x -> float_of_int x) (from n) in
  let rec exp_terms' acc (Cons (h, t) as l) =
    let h' = x *. acc /. h in
    let t' = lazy(exp_terms' h' @@ tl l)
    in Cons(h', t')
  in Cons(1., lazy (exp_terms' 1. (from_float 1)))

(* Answer to 2b *)
let approx_exp_2b =
  exp_terms 1.1
  |> take 20
  |> List.fold_left (fun acc curr -> acc +. curr) 0.