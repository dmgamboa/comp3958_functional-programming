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
    print_string "Path: 0 " ;
    List.iter (Printf.printf("-> %d ")) path ;
    print_endline "-> 0";