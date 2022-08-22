open Printf
open Sc

(*

Usage: 
  
  cat filename | ./fd_dist
  cat filename | ./fd_dist --safe

  ./fd_dist < filename
  ./fd_dist --safe < filename

*)

let report_facet_degree_distrib com =

  let max_facet_deg =
    Hashtbl.fold(fun i s acc -> 
      max acc (Sc.S.cardinal s)
    ) com.T.nd 0
  in
  let num_nodes = Hashtbl.length com.nd in

  let hist = Array.make (max_facet_deg+1) 0 in
  Hashtbl.iter(fun _ s -> 
    let deg = Sc.S.cardinal s in
    hist.(deg) <- hist.(deg) + 1
  ) com.T.nd;

  Array.iteri (fun deg count -> 
    if count > 0 then
      Printf.printf "%i %g\n" deg (float count /. float num_nodes)
  ) hist


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
  report_facet_degree_distrib com
   
