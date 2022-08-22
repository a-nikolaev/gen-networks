open Printf
open Sc

(*

Usage: 
  
  cat filename | ./ed_dist
  cat filename | ./ed_dist --safe

  ./ed_dist < filename
  ./ed_dist --safe < filename

*)

let report_edge_degree_distrib com =
 
  (* compute skeleton *)
  let ht = Hashtbl.create 10 in
  fold_facets (fun a _ () ->
    fold_nodes_in_facet (fun i () -> 
        fold_nodes_in_facet (fun j () -> 
            if (i < j) then Hashtbl.replace ht (i,j) () 
          )
          (com, a) ()
      ) 
      (com, a) () 
  ) com ();

  (* edge degrees of nodes *)
  let ht_deg = Hashtbl.create 10 in
  let get_deg i = 
    try Hashtbl.find ht_deg i with
    | Not_found -> 0
  in
  let inc_deg i =
    Hashtbl.replace ht_deg i (get_deg i + 1)
  in
  ht |> Hashtbl.iter (fun (i,j) () ->
    inc_deg i;
    inc_deg j
  );

  let max_edge_deg =
    Hashtbl.fold (fun i deg acc -> 
      max acc deg
    ) ht_deg 0
  in
  let num_nodes = nodes_num com in

  let hist = Array.make (max_edge_deg+1) 0 in
  Hashtbl.iter(fun i deg -> 
    hist.(deg) <- hist.(deg) + 1
  ) ht_deg;

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
  report_edge_degree_distrib com
   
