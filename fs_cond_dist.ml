
open Printf
open Sc

let report arr num_facets =
  Array.iter (fun (size, count) -> 
    Printf.printf "%i %g\n" size (float count /. float num_facets)
  ) arr

(* report facet size distributions for facets that contain
   at least one node of  lo_deg <= degree <= hi_deg  *)
let report_facet_size_cond_dist opt_lo_deg opt_hi_deg com =

  let a_set = 
    let good =
      let lo_good = 
        match opt_lo_deg with 
        | Some lo -> (fun deg -> lo <= deg)
        | None -> (fun _ -> true)
      in
      let hi_good = 
        match opt_hi_deg with 
        | Some hi -> (fun deg -> deg <= hi)
        | None -> (fun _ -> true)
      in
      (fun d -> hi_good d && lo_good d)
    in
    Sc.fold_nodes (fun i nbhd acc ->
        let deg = Sc.facet_degree i com in
        if good deg then begin
          Sc.S.union nbhd acc
        end else
          acc
    ) com Sc.S.empty
  in

  let ht_freq = Hashtbl.create 10 in
  let get size = try Hashtbl.find ht_freq size with Not_found -> 0 in
  let add size = Hashtbl.replace ht_freq size (get size + 1) in
  let num_facets = S.fold (fun a acc ->
      let size = Sc.facet_size a com in
      add size;
      acc + 1
    ) a_set 0
  in

  let arr = Array.make (Hashtbl.length ht_freq) (0, 0) in
  ignore (Hashtbl.fold (fun size freq i_acc -> arr.(i_acc) <- (size, freq); i_acc+1) ht_freq 0);
  Array.sort (fun (s1,_) (s2,_) -> compare s1 s2) arr;
  
  report arr num_facets


let report_facet_size_cond_dist_old com =

    let max_facet_degree = Sc.fold_nodes (fun i nbhd acc -> max acc (Sc.facet_degree i com)) com 0 in

    let degrees = [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512; 1024; 2048 ] |> List.filter (fun d -> d < max_facet_degree*2) in

    let max_facet_size =
      Hashtbl.fold(fun a s acc -> 
        max acc (Sc.S.cardinal s)
      ) com.ft 0
    in
    let num_facets = Hashtbl.length com.ft in

    let full_hist = Array.make (max_facet_size+1) 0 in
    let con_hists = Array.init (List.length degrees) (fun d -> Array.make (max_facet_size+1) 0) in
    let con_sum_degs = Array.make (List.length degrees) 0 in
    let con_num_nodes = Array.make (List.length degrees) 0 in
    let con_num_facets = Array.make (List.length degrees) 0 in
    
    Hashtbl.iter (fun a ft ->
        let size = Sc.facet_size a com in
        full_hist.(size) <- full_hist.(size) + 1

      ) com.ft;

    Hashtbl.iter (fun n nbrs ->
              
        let d = Sc.facet_degree n com in

        let find_deg d = 
          let rec next i deg_ls =
            match deg_ls with
            | dd :: tl when d <= dd -> Some (i,d)
            | _ :: tl -> next (i+1) tl
            | [] -> None
          in
          next 0 degrees
        in

        match find_deg d with
        | Some (i, dd) ->
            con_sum_degs.(i) <- con_sum_degs.(i) + d;
            con_num_nodes.(i) <- con_num_nodes.(i) + 1;

            Sc.S.fold (fun a () ->
              let size = Sc.facet_size a com in 
              con_hists.(i).(size) <- con_hists.(i).(size) + 1;
              con_num_facets.(i) <- con_num_facets.(i) + 1
            ) nbrs ()
        | None -> Printf.printf "Error: Degree %i is too large.\n" d

      ) com.nd;
        
    Printf.printf "# s\tP(s)\t";
    degrees |> List.iteri (fun i deg ->
      Printf.printf "%f\t" (float con_sum_degs.(i) /. float con_num_nodes.(i)));
    Printf.printf "\n";

    for size = 1 to max_facet_size do 
      ( 
        let p_full = float full_hist.(size) /. float num_facets in
        Printf.printf "%i\t%f\t" size p_full;

        let p_full_m1 = float full_hist.(size-1) /. float num_facets in
        
        degrees |> List.iteri (fun i deg ->
          let c = con_hists.(i).(size) in
          let tot = con_num_facets.(i) in

          let p_con = (float c /. float tot) in

          (* Printf.printf "%f\t" (p_con /. p_full_m1) *)
          Printf.printf "%f\t" (p_con) 
        );
        Printf.printf "\n"
      )
    done


let _ =

  Random.self_init();
  
  let open Opt in
  let flag, param, _ =
    Opt.scan 
      (* flags  *) [Long "safe"] 
      (* params *) [Long "lo"; Long "hi"]
      Sys.argv 1
  in
  
  let opt_lo = 
    match param (Long "lo") with
    | Some (OK s) -> Some (int_of_string s)
    | _ -> None
  in
  
  let opt_hi = 
    match param (Long "hi") with
    | Some (OK s) -> Some (int_of_string s)
    | _ -> None
  in

  let safe = flag (Long "safe") in

  let com = Sc.parse_from_chan safe stdin in

  (*
  report_facet_size_cond_dist_old com;
  *)

  report_facet_size_cond_dist opt_lo opt_hi com;
