(* GAMBOA, Donna A01207448 *)

let () = 
    let v0 = Sys.argv.(1) in
    let file_name = Sys.argv.(2) in
    let tree = Prim.min_tree v0 (Utils.read_graph file_name) in
    let print_edge (u, v, w) = Printf.printf("<%s, %s, %d> ") u v w in
    Printf.printf ("minimum weight: %d\nedges: ") (Utils.sum_weights tree) ;
    List.iter (print_edge) tree ;
    Printf.printf ("\n") ;