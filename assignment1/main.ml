(* GAMBOA, Donna A01207448 *)

include Tsp

let () =
    Random.init 32768 ;
    let file_name = Sys.argv.(1) in
    let iterations = Sys.argv.(2) |> int_of_string in
    let t0 = 100.0 in
    let tf = 0.99 in
    let ti = (iterations / 500) + 1 in
    let (length, path) = Tsp.run file_name (t0, tf, ti) iterations in
    Printf.printf ("Distance: %.0f\n") length ;
    (* Print itineraryt o match the solution output format: 
    1. Start / end vertex included
    2. Each vertex labelled as their index + 1 *)
    print_endline "Path: 1" ;
    List.iter (Printf.printf("%d\n")) (path |> List.map (fun x -> x + 1));
    print_endline "1";