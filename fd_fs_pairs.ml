open Printf
open Sc

(*

Usage: 
  
  cat filename | ./fd_fs_pairs
  cat filename | ./fd_fs_pairs --safe

  ./fd_fs_pairs < filename
  ./fd_fs_pairs --safe < filename

*)

let report_all_fd_fs_pairs com =

  Sc.fold_facets (fun a facet () ->
    let ft_size = Sc.facet_size a com in
    Sc.fold_nodes_in_facet (fun i () ->
      let deg = Sc.facet_degree i com in
      Printf.printf "%i %i\n" deg ft_size
    ) (com, a) ()
  ) com ()

let _ =

  Random.self_init();
  
  let open Opt in
  let flag, _, _ =
    Opt.scan 
      (* flags  *) [Long "safe"] 
      (* params *) []
      Sys.argv 1
  in
  
  let safe = flag (Long "safe") in

  let com = Sc.parse_from_chan safe stdin in
  report_all_fd_fs_pairs com
   
