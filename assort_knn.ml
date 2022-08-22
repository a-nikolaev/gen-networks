open Printf
open Sc

(*

Assortativity - Neighbor connectivity
https://en.wikipedia.org/wiki/Assortativity#Neighbor_connectivity

          knn(k) = sum_{t} t P(t|k)

(Computed two ways, for edge degree and for facet degree)


Usage: 
  
  cat filename | ./assort_knn --f=file --e=file
  cat filename | ./assort_knn --safe --f=file --e=file

  ./assort_knn < filename --f=file --e=file
  ./assort_knn --safe < filename --f=file --e=file

*)

let report_neighbors_degrees com ffile_opt efile_opt =
 
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
 
  let ht_ft_degsum = Hashtbl.create 10 in (* sum of degrees of the neighbors for node of size k *)
  let ht_ft_num = Hashtbl.create 10 in    (* number of neighbors for node of size k *)
  let ht_ed_degsum = Hashtbl.create 10 in
  let ht_ed_num = Hashtbl.create 10 in

  let get ht i = 
    try Hashtbl.find ht i with
    | Not_found -> 0
  in
  let inc_by ht i dv =
    Hashtbl.replace ht i (get ht i + dv)
  in

  let max_ft_deg = ref 0 in
  let max_ed_deg = ref 0 in

  ht |> Hashtbl.iter (fun (i,j) () ->
    let ft_deg_i = Sc.facet_degree i com in
    let ft_deg_j = Sc.facet_degree j com in
    let ed_deg_i = get_deg i in
    let ed_deg_j = get_deg j in

    inc_by ht_ft_degsum ft_deg_i ft_deg_j;
    inc_by ht_ft_num ft_deg_i 1;
    
    inc_by ht_ft_degsum ft_deg_j ft_deg_i;
    inc_by ht_ft_num ft_deg_j 1;
    
    inc_by ht_ed_degsum ed_deg_i ed_deg_j;
    inc_by ht_ed_num ed_deg_i 1;
    
    inc_by ht_ed_degsum ed_deg_j ed_deg_i;
    inc_by ht_ed_num ed_deg_j 1;

    max_ft_deg := max (!max_ft_deg) (max ft_deg_i ft_deg_j);
    max_ed_deg := max (!max_ed_deg) (max ed_deg_i ed_deg_j);
  );

  ( match ffile_opt with
    | Some (Opt.OK file) -> 
      ( 
        let oc = 
          if file <> "-" then
            open_out file 
          else
            stdout
        in

        Printf.fprintf oc "# degree Knn-facet-degree\n";

        for d = 0 to (!max_ft_deg) do
          let nf = get ht_ft_num d in

          if nf > 0 then begin
            let prob_f = string_of_float (float (get ht_ft_degsum d) /. float nf) in
            Printf.fprintf oc "%i %s\n" d prob_f
          end

        done;

        if file <> "-" then
          close_out oc;
      )
    | _ -> ()
  );
  
  ( match efile_opt with
    | Some (Opt.OK file) -> 
      ( 
        let oc = 
          if file <> "-" then
            open_out file 
          else
            stdout
        in

        Printf.fprintf oc "# degree Knn-edge-degree\n";

        for d = 0 to (!max_ed_deg) do
          let ne = get ht_ed_num d in

          if ne > 0 then begin
            let prob_e = string_of_float (float (get ht_ed_degsum d) /. float ne) in
            Printf.fprintf oc "%i %s\n" d prob_e
          end

        done;

        if file <> "-" then
          close_out oc;
      )
    | _ -> ()
  )

  (*
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
  *)

let _ =

  Random.self_init();
  
  let open Opt in
  let flag, param, _ =
    Opt.scan 
      (* flags  *) [Long "safe"] 
      (* params *) [Long "f"; Long "e"]
      Sys.argv 1
  in
  
  let safe = flag (Long "safe") in

  let com = Sc.parse_from_chan safe stdin in
  report_neighbors_degrees com (param (Long "f")) (param (Long "e"))
  


