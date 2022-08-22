
let facet_intersection_card sc a b =
  if a = b then
    Sc.S.cardinal (Sc.T.ft a sc)
  else
    let fta = Sc.T.ft a sc in
    let ftb = Sc.T.ft b sc in
    Sc.S.(cardinal (inter fta ftb))

let q_structure opt_ecc_file sc = 

  (* Eccentricity of each facet *)
  let ht_ecc = Hashtbl.create 10 in

  (* find the overlap between each pair of facets, call them "links" *)
  let ht_links = Hashtbl.create 10 in
  Sc.fold_nodes (fun i nbrs () ->
    nbrs |> Sc.S.iter (fun a ->
      nbrs |> Sc.S.iter (fun b ->
        if a <= b && not (Hashtbl.mem ht_links (a,b)) then
          let overlap = facet_intersection_card sc a b in
          Hashtbl.replace ht_links (a,b) overlap
      )
    )
  ) sc ();

  (* radix sort for all links *)
  let ht_grouped = Hashtbl.create 10 in
  let get_group link =
    try
      Hashtbl.find ht_grouped link 
    with
    | Not_found -> []
  in

  let add_to_group link ab =
    let ls = get_group link in
    Hashtbl.replace ht_grouped link (ab :: ls)
  in
  
  (* go over all links and add them into groups, compute maximum link size *)
  let max_link = Hashtbl.fold (fun ab link acc ->
      add_to_group link ab;
      max acc link
    ) 
    ht_links 0
  in

  (* joining connected components via their links, starting with the thickest *)
  let du = Union_find.empty () in

    
  Printf.printf "# q+1  facet_num  Sum_ft_num   num-cc   avg-cc-size   max-cc-size       +ft_num   +join    Sum_join +Rjoin   Sum_Rjoin\n"; 

  (* number of added facets of size >= link, i.e. = CCMF * #facets *)
  let added_facet_num = ref 0 in
  let all_joins = ref 0 in
  let all_real_joins = ref 0 in

  for link = max_link downto 1 do
    let pairs = get_group link in
    
    let this_level_new_facet_num = ref 0 in
    let this_level_joins = ref 0 in
    let this_level_real_joins = ref 0 in

    (* add new facets of size=link *)
    pairs |> List.iter (fun (a,b) ->
      if a = b then (
        added_facet_num := !added_facet_num + 1;
        this_level_new_facet_num := !this_level_new_facet_num + 1;
        Union_find.add du a
      )
    );
    (* join them *)
    pairs |> List.iter (fun (a,b) ->
      if a < b then begin
        let cc_size_a = Union_find.size du a in
        let cc_size_b = Union_find.size du b in
        assert (cc_size_a = 1 || Hashtbl.mem ht_ecc a);
        assert (cc_size_b = 1 || Hashtbl.mem ht_ecc b);
        
        let current_q = link - 1 in
        
        if cc_size_a = 1 then begin
          let dim = Sc.facet_size a sc - 1 in 
          let ecc = float (dim - current_q) /. float (current_q + 1) in
          Hashtbl.replace ht_ecc a ecc
        end;
        
        if cc_size_b = 1 then begin
          let dim = Sc.facet_size b sc - 1 in 
          let ecc = float (dim - current_q) /. float (current_q + 1) in
          Hashtbl.replace ht_ecc b ecc
        end;

        this_level_joins := !this_level_joins + 1;
        all_joins := !all_joins + 1;
        
        let root_e_a = Union_find.find_root_entry du a in
        let root_e_b = Union_find.find_root_entry du b in
        if root_e_a.v <> root_e_b.v then begin
          this_level_real_joins := !this_level_real_joins + 1;
          all_real_joins := !all_real_joins + 1;
        end;

        Union_find.union du a b
      end
    );

    let facets = Sc.facet_num sc in

    let avg_size, ext_avg_size, max_size = 
      let ls = Union_find.all_sizes du in
      
      let avg, ext_avg = 
        let sum, num = 
          List.fold_left (fun (sum, num) s ->
              (sum + s, num + 1)
            ) (0, 0) ls
        in
        assert (num = du.Union_find.num);
        (*
         num : number of connected components of the facets that were added
         small_singletons : number of not yet added facets (each is potentially a CC contributing sum+=1, num+=1)
         *)
        let small_singletons = facets - !added_facet_num in
        (float sum /. float num, float (sum + small_singletons) /. float (num + small_singletons)) 
      in
      
      let arr = Array.of_list ls in
      Array.sort compare arr;

      let len = Array.length arr in
      let maximum = arr.(len - 1) in

      (avg, ext_avg, maximum)
    in

    let ext_cc_num = du.Union_find.num + (facets - !added_facet_num) in

    Printf.printf "%-8i %-8i %-8i     %-12g %-12g %-12g     %-8i %-8i %-8i %-8i %-8i\n%!" 
      link
      facets 
      (!added_facet_num)            (* = CCMF(s) * m *)
      
      (float du.Union_find.num) 
      (avg_size) 
      (float max_size)

      (!this_level_new_facet_num)   (* = P(s) m *)
      (!this_level_joins)
      (!all_joins)
      (!this_level_real_joins)
      (!all_real_joins)
  done;

  (* save eccentricity to the file *)
  match opt_ecc_file with
  | None -> ()
  | Some ecc_file ->
      let arr = Array.make (Hashtbl.length ht_ecc) 0.0 in
      Hashtbl.fold (fun a ecc i -> 
          arr.(i) <- ecc;
          i + 1
        ) ht_ecc 0 |> ignore;
      Array.sort compare arr;

      (* write the distribution *)
      let oc = open_out ecc_file in
      let tot_num = Array.length arr in

      Array.fold_left (fun (i, acc_cpf) _ ->
        let cpf = float (i+1) /. float tot_num in
        let ecc = arr.(i) in
        if i = tot_num-1 || arr.(i) < arr.(i+1) then
          Printf.fprintf oc "%g \t %g\n%!" ecc cpf;
        (i+1, cpf)
      ) (0, 0.0) arr |> ignore;

      (* mean and variance *)
      let mean = 
        let sum = Array.fold_left (fun sum ecc -> sum +. log ecc) 0.0 arr in
        sum /. float tot_num
      in
      let var = 
        let sum_sq = Array.fold_left (fun sum ecc -> sum +. (let x = (log ecc -. mean) in x*.x)) 0.0 arr in
        sum_sq /. float (tot_num-1)
      in
      Printf.fprintf oc "# mean-log-ecc %g\n" mean;
      Printf.fprintf oc "# variance-log-ecc %g\n" var;
      Printf.fprintf oc "# stddev-log-ecc %g\n%!" (sqrt var);

      close_out oc

let () =

  let open Opt in
  let flag, param, _ =
    Opt.scan 
      (* flags  *) [Long "safe"] 
      (* params *) [Long "ecc"]
      Sys.argv 1
  in
  
  let safe = flag (Long "safe") in
  let opt_ecc_file = match param (Long "ecc") with
    | Some (OK s) -> Some s
    | _ -> None
  in

  let com = Sc.parse_from_chan safe stdin in

  q_structure opt_ecc_file com
  
