
let max_facet_size com = 
  Sc.fold_facets (fun a _ acc ->
    let size = Sc.facet_size a com in
    max size acc
  ) com 0

let max_facet_degree com = 
  Sc.fold_nodes (fun i _ acc ->
    let deg = Sc.facet_degree i com in
    max deg acc
  ) com 0


(* accumulate facet degree correlation deterministically *)
let accum ll com =
  let max_ft_sz = max_facet_size com in
  let max_ft_deg = max_facet_degree com in

  let ht1 = Hashtbl.create 100 in
  let ht2 = Hashtbl.create 100 in

  let add ht key v =
    let sum, num = try Hashtbl.find ht key with Not_found -> (0.0, 0.0) in
    Hashtbl.replace ht key (sum +. v, num +. 1.0)
  in
  
  let average ht key =
    try 
      let sum, num = Hashtbl.find ht key in
      Some (sum /. num)
    with 
    | Not_found -> None
  in
    
  Sc.fold_facets (fun a facet () ->
    
    let s = Sc.facet_size a com in

    (* E[ (k1+k2+...k_{s-1})^(L-1) | s,k ]  *)
    (* let ht_seen = Hashtbl.create 1 in *)
    facet |> Sc.S.iter (fun i ->
      let di = Sc.facet_degree i com in

      (*
      if (s = 7 && di = 768) then begin
        Sc.fprint_set stderr facet;
        Printf.fprintf stderr "\n";
        facet |> Sc.S.iter (fun j -> 
          let dj = Sc.facet_degree j com in
          Printf.fprintf stderr "%i: %i  %s\n" j dj (if i=j then "<---" else "");
        )
      end;
      *)

      if true (* not (Hashtbl.mem ht_seen di) *) then
      begin
          (* Hashtbl.replace ht_seen di (); *)

          let sum_other_deg = 
            Sc.S.fold (fun j acc ->
              if i <> j then
                let dj = Sc.facet_degree j com in
                acc + dj      
              else
                acc
            ) facet 0
          in

          let v = (float sum_other_deg) ** (float ll -. 1.0) in

          add ht1 (s, (log (float di) /. log (2.0)) |> floor |> int_of_float) v
      end
    );

    (*  E[ k^(L-1) | s ], to be multiplied by (s-1)^(L-1)  *)
    Sc.S.iter (fun j ->
      let dj = Sc.facet_degree j com in
      let v = (float dj) ** (float ll -. 1.0) in
      add ht2 s v
    ) facet;

  ) com ();

  for s = 1 to max_ft_sz do
    match average ht2 s with
    | Some avg2 -> 
        let e2 = ((float s) ** (float ll -. 1.0)) *. avg2 in

        for k = 1 to max_ft_deg do
          match average ht1 (s,k) with
          | Some e1 ->
              Printf.printf "%i\t%i\t%g\t%g\n" s k e1 e2
          | None -> ()
        done;

        Printf.printf "\n"
    | None -> ()
  done

let () =

  let open Opt in
  let flag, param, _ =
    Opt.scan 
      (* flags  *) [Long "safe"] 
      (* params *) [Long "ll"]
      Sys.argv 1
  in
  
  let safe = flag (Long "safe") in

  match param (Long "ll") with
  | Some (Opt.OK ll) ->
      let com = Sc.parse_from_chan safe stdin in
      accum (int_of_string ll) com
  | _ ->
      Printf.printf "Usage:\n\t ./fd_corr_more [--safe] --ll={1,2,3,...} < scfile \n\n"


