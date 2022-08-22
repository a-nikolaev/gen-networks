
open Printf
open Sc

(*

Usage: 
  
  cat filename | ./fs_dist
  cat filename | ./fs_dist --safe

  ./fs_dist < filename
  ./fs_dist --safe < filename

*)

let report_facet_size_distrib com =

  let max_facet_size =
    Hashtbl.fold(fun a s acc -> 
      max acc (Sc.S.cardinal s)
    ) com.T.ft 0
  in
  let num_facets = Hashtbl.length com.T.ft in

  let hist = Array.make (max_facet_size+1) 0 in
  Hashtbl.iter(fun _ s -> 
    let size = Sc.S.cardinal s in
    hist.(size) <- hist.(size) + 1
  ) com.T.ft;

  Array.iteri (fun size count -> 
    if count > 0 then
      Printf.printf "%i %g\n" size (float count /. float num_facets)
  ) hist


let _ =

  Random.self_init();
  
  let open Opt in
  let flag, _, _ =
    Opt.scan 
      (* flags  *) [Long "safe"; Long "fpn"] 
      (* params *) []
      Sys.argv 1
  in
  
  let safe = flag (Long "safe") in
  let needs_fpn = flag (Long "fpn") in (* facets per node ration for Basu's growth model *)

  let com = Sc.parse_from_chan safe stdin in
 
  if needs_fpn then begin 
    let fpn = float (Sc.facet_num com) /. float (Sc.nodes_num com) in
    Printf.printf "%g\n" fpn
  end;

  report_facet_size_distrib com

