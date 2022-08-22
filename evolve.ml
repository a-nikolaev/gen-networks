
open Sc

type model =
  | ACL of (int * (float * int * int))
  | ACdistL of (int * (float * ((int*float) list) * int))
  | SizeDist of (float * ((int * float) list))
  | ACLSizeDist of (int * (float * int * int) * ((int * float) list))

let sample_ft com =
  match sample_facet_id com with
  | None -> failwith "evolve: sample_facet_id: cannot sample a facet"
  | Some a -> a

(* let output = true *)

(* 1 *)
let mut_add_person com = 
  let a = sample_ft com in
  
  (*(* *) if output then Printf.printf "\t\t\t\tAdd person \n";*)
  mut_add_new_node_to_facet_id a com


let select_from_facet_uniform prob_to_take facet =
  S.fold (fun e acc -> if Random.float 1.0 < prob_to_take then S.add e acc else acc) facet S.empty 

let select_from_facet com prob_to_take facet =
  select_from_facet_uniform prob_to_take facet

(* 2 *)
let mut_add_project com = 

  let rec sample_bunch_of_facets prob acc = 
    if Random.float 1.0 < prob then
      sample_bunch_of_facets prob (sample_ft com :: acc)
    else
      acc
  in
  (* sample at least twice and maybe more facets *)
  let ls = sample_bunch_of_facets 0.5 [sample_ft com; sample_ft com] in
  let all_nodes = List.fold_left (fun acc a -> S.union acc (Sc.T.ft a com)) S.empty ls in
  let new_ft = select_from_facet com 0.333333333 all_nodes in
 
  (*
  (* *) if output then
        ( Printf.printf "\t\t\t\tAdd project ";
          fprint_set stdout new_ft;
          Printf.printf "  [from %i joined " (List.length ls);
          fprint_set stdout all_nodes;
          Printf.printf "]\n";
        );
  *)

  ignore (mut_add_facet new_ft com)



(* 3 *)
let mut_split_project com =
  let a = sample_ft com in
  let ft0 = Sc.T.ft a com in

  let ft1, ft2 = 
    S.fold 
      (fun i (left, right) -> 
        if Random.int 2 = 0 then (S.add i left, right) else (left, S.add i right)
      ) 
      ft0 (S.empty, S.empty) 
  in
  
  (*
  (* *) if output then
        ( Printf.printf "\t\t\t\tSplit project ";
          fprint_set stdout ft0;
          Printf.printf " -> ";
          fprint_set stdout ft1;
          Printf.printf " + ";
          fprint_set stdout ft2;
          Printf.printf "\n";
        );
  *)

  mut_remove_facet_id a com;
  ignore (mut_add_facet ft1 com);
  ignore (mut_add_facet ft2 com)


(* 4 *)
let mut_add_separate_person com = 
  
  (*(* *) if output then Printf.printf "\t\t\t\tAdd separate person \n";*)
   
  let i = S.max_elt com.T.nodes + 1 in
  
  let facet = S.of_list [i] in

  ignore (mut_add_facet facet com)


(* 5 - union (ACL1) *)
let mut_add_project_with_extra_person (alpha, cc, ll) com = 

  let rec sample_bunch_of_facets number acc = 
    if number > 0 then
      sample_bunch_of_facets (number-1) (sample_ft com :: acc)
    else
      acc
  in
  (* sample at least twice and maybe more facets *)
  let ls = sample_bunch_of_facets ll [] in
  let all_nodes = List.fold_left (fun acc a -> S.union acc (Sc.T.ft a com)) S.empty ls in
  let new_ft = select_from_facet com (alpha *. 1.0 /. float ll) all_nodes in
 
  (* adding n extra new nodes to it starting at the value i *)
  let rec add_extra_nodes i n ft =
    if n > 0 then
      let nft = S.add i ft in
      add_extra_nodes (i+1) (n-1) nft
    else
      ft
  in
  
  let real_new_ft = add_extra_nodes (S.max_elt com.T.nodes + 1) cc new_ft in

  (*
  (* printing *)
  Sc.fprint_normal stdout com;
  Printf.printf "Facets: ";
  List.iter (fun a -> Sc.fprint_set stdout (Sc.T.ft a com); Printf.printf " ") ls;
  Printf.printf "\nNew facet: ";
  Sc.fprint_set stdout real_new_ft;
  Printf.printf "\n";
  *)

  (*
  (* *) if output then
        ( Printf.printf "\t\t\t\tAdd project ";
          fprint_set stdout new_ft;
          Printf.printf "  [from %i joined " (List.length ls);
          fprint_set stdout all_nodes;
          Printf.printf "]\n";
        );
  *)

  let response =  mut_add_facet real_new_ft com in
  (ls, response) (* returns 
                    the list of facet ids that were sampled, and mut_add_facet response *)
  

(* 6 - multiset (ACL2) *)
let mut_add_project_with_extra_person_2 (alpha, cc, ll) com = 

  let rec sample_bunch_of_facets number acc = 
    if number > 0 then
      sample_bunch_of_facets (number-1) (sample_ft com :: acc)
    else
      acc
  in
  (* sample ll facets *)
  let ls = sample_bunch_of_facets ll [] in
  let new_ft = 
    List.fold_left (fun acc a -> 
      let ft = Sc.T.ft a com in
      let sampled_nodes = select_from_facet com (alpha *. 1.0 /. float ll) ft in
      S.union acc sampled_nodes
    ) 
    S.empty ls 
  in
 
  (* adding n extra new nodes to it starting at the value i *)
  let rec add_extra_nodes i n ft =
    if n > 0 then
      let nft = S.add i ft in
      add_extra_nodes (i+1) (n-1) nft
    else
      ft
  in
  
  let real_new_ft = add_extra_nodes (S.max_elt com.T.nodes + 1) cc new_ft in

  (*
  (* printing *)
  Sc.fprint_normal stdout com;
  Printf.printf "Facets: ";
  List.iter (fun a -> Sc.fprint_set stdout (Sc.T.ft a com); Printf.printf " ") ls;
  Printf.printf "\nNew facet: ";
  Sc.fprint_set stdout real_new_ft;
  Printf.printf "\n";
  *)

  (*
  (* *) if output then
        ( Printf.printf "\t\t\t\tAdd project ";
          fprint_set stdout new_ft;
          Printf.printf "  [from %i joined " (List.length ls);
          fprint_set stdout all_nodes;
          Printf.printf "]\n";
        );
  *)

  let response =  mut_add_facet real_new_ft com in
  (ls, response) (* returns 
                    the list of facet ids that were sampled, and mut_add_facet response *)
  

(* 7 - multiset, all facets are distinct (ACL3) *)
let mut_add_project_with_extra_person_3 (alpha, cc, ll) com = 

  (* sample list of ll facets *)
  let ls = 
    if Sc.facet_num com <= ll then
      Sc.fold_facets (fun a _ acc -> a::acc) com []
    else
      let rec sample_bunch_of_facets number acc = 
        if number > 0 then begin
          let a = sample_ft com in
            if List.mem a acc then
              sample_bunch_of_facets (number) (acc)
            else
              sample_bunch_of_facets (number-1) (a :: acc)
        end else
          acc
      in
      sample_bunch_of_facets ll []
  in

  let new_ft = 
    List.fold_left (fun acc a -> 
      let ft = Sc.T.ft a com in
      let sampled_nodes = select_from_facet com (alpha *. 1.0 /. float ll) ft in
      S.union acc sampled_nodes
    ) 
    S.empty ls 
  in
 
  (* adding n extra new nodes to it starting at the value i *)
  let rec add_extra_nodes i n ft =
    if n > 0 then
      let nft = S.add i ft in
      add_extra_nodes (i+1) (n-1) nft
    else
      ft
  in
  
  let real_new_ft = add_extra_nodes (S.max_elt com.T.nodes + 1) cc new_ft in

  (*
  (* printing *)
  Sc.fprint_normal stdout com;
  Printf.printf "Facets: ";
  List.iter (fun a -> Sc.fprint_set stdout (Sc.T.ft a com); Printf.printf " ") ls;
  Printf.printf "\nNew facet: ";
  Sc.fprint_set stdout real_new_ft;
  Printf.printf "\n";
  *)

  (*
  (* *) if output then
        ( Printf.printf "\t\t\t\tAdd project ";
          fprint_set stdout new_ft;
          Printf.printf "  [from %i joined " (List.length ls);
          fprint_set stdout all_nodes;
          Printf.printf "]\n";
        );
  *)

  let response =  mut_add_facet real_new_ft com in
  (ls, response) (* returns 
                    the list of facet ids that were sampled, and mut_add_facet response *)
  
(* 8 - Basu growth *)
(* 
  Problems with the original Basu's algorithm:
  - mergev could be larger than the new facet size s (b/c nv could still be smaller than the current number of nodes |V|).
     So, if the implementater is not careful, attempts to merge more than s nodes can lead to a bug or incorrect facet size.
  - mergev could be larger than |V|.
    You want mergev to be clamped (at least) between 0 and min(s, |V|).
  - Moreover, if the network is made of one facet {1, 2, ... k}, |V| = k, |F|=1,
    Adding a new facet may be impossible, say s <= k, so mergev = s, and you simply merge all nodes, and
    if the only facet in the network has the maximum facet size allowed by the distribution, the growth process
    can get stuck in this degenerate state indefinitely.
    Practically, it makes sense to choose mergev no more than (|V|-1) to allow the growth break out of this problematic situation.
    Once you have more than one facet, such a problem doe snot really exist.

    >>> So, to summarize, it is practical to clamp mergev between 0 and min(s, |V|-1)  <<<
    This clamping does not affect the growth when |V| and |F| is large, but helps in the earl ystages of the network growth.

  - When algorithm creates a new facet of size 1, it is likely to be subsumed by existing nodes (and the supplied proof does 
    not account for that). So the generated distributions become skewed with the number of facet of size 1 being reduced.
    (It is worth noting though that subsumptions don't break the targeted ratio between facets and nodes (|F| = C |V|),
    because on each step the targeted number of nodes (nv) is generated for the current number of facets. The algorithm
    is able to dynamically adjust the ratio between the number of nodes and facets despite subsumptions.)
 
  - Cannot control the power-law exponent (given fixed facet size distribution and |F|/|V| ratio).
 *)
let mut_add_project_basu (fpn, fs_dist) com = 

  let s = 
    match Common.Sample.from_list_prob fs_dist with
    | Some s -> s
    | None -> failwith "Evolve: mut_add_project_basu: facet size distribution is empty"
  in

  let n = nodes_num com in
  let m = facet_num com in

  let clamp low high x = x |> max low |> min high in

  (* override s *)
  let s = min (n/2+1) s in

  let new_n = (float (m + 1) /. fpn) |> ceil |> int_of_float in
  (* let old_nodes_num = (clamp 0 (min s (n-1))) (s - (new_n - n)) in *)
  (*
  if new_n < n then begin
    Printf.fprintf stderr "n = %i, new_n = %i, s = %i\n%!" n new_n s
  end;
  let old_nodes_num = max 0 (s - (new_n - n)) in
  *)
  let old_nodes_num = clamp 0 s (s - (new_n - n)) in 

  let rec add_old num set =
    if num > 0 then
      let i = Sc.sample_node_pref com in
      if Sc.S.mem i set then
        add_old num set
      else
        add_old (num-1) (Sc.S.add i set) 
    else
      set
  in

  let facet0 =
    if old_nodes_num = n then 
      com.Sc.T.nodes 
    else 
      add_old old_nodes_num Sc.S.empty
  in
  
  assert (S.cardinal facet0 = old_nodes_num);

  (* adding n extra new nodes to it starting at the value i *)
  let rec add_extra_nodes i n ft =
    if n > 0 then
      let nft = S.add i ft in
      add_extra_nodes (i+1) (n-1) nft
    else
      ft
  in

  let real_new_ft = add_extra_nodes (S.max_elt com.T.nodes + 1) (s - old_nodes_num) facet0 in
  
  assert (S.cardinal real_new_ft = s); 
  (*
  if new_n < n then begin
    Printf.fprintf stderr "\t\t %i <> %i\n%!" (S.cardinal real_new_ft) s
  end;
  *)

  (*
  (* printing *)
  Sc.fprint_normal stdout com;
  Printf.printf "Facets: ";
  List.iter (fun a -> Sc.fprint_set stdout (Sc.T.ft a com); Printf.printf " ") ls;
  Printf.printf "\nNew facet: ";
  Sc.fprint_set stdout real_new_ft;
  Printf.printf "\n";
  *)

  (*
  (* *) if output then
        ( Printf.printf "\t\t\t\tAdd project ";
          fprint_set stdout new_ft;
          Printf.printf "  [from %i joined " (List.length ls);
          fprint_set stdout all_nodes;
          Printf.printf "]\n";
        );
  *)

  let response =  mut_add_facet real_new_ft com in
  ([], response) (* returns 
                    the list of facet ids that were sampled, and mut_add_facet response *)

(* 6(STAR) - multiset (ACL2) variant, where we try to mimic size distribution *)
let mut_add_project_with_extra_person_2_size (alpha, cc, ll) size com = 

  let rec sample_bunch_of_facets number acc = 
    if number > 0 then
      sample_bunch_of_facets (number-1) (sample_ft com :: acc)
    else
      acc
  in
  (* sample ll facets *)
  let ls = sample_bunch_of_facets ll [] in
  let new_ft = 
    List.fold_left (fun acc a -> 
      let ft = Sc.T.ft a com in
      let sampled_nodes = select_from_facet com (alpha *. 1.0 /. float ll) ft in
      S.union acc sampled_nodes
    ) 
    S.empty ls 
  in
 
  (* adding n extra new nodes to it starting at the value i *)
  let rec add_extra_nodes i n ft =
    if n > 0 then
      let nft = S.add i ft in
      add_extra_nodes (i+1) (n-1) nft
    else
      ft
  in
  
  let sampled_num = Sc.S.cardinal new_ft in

  let real_new_ft = add_extra_nodes (S.max_elt com.T.nodes + 1) (max 1 (size - sampled_num)) new_ft in

  (*
  (* printing *)
  Sc.fprint_normal stdout com;
  Printf.printf "Facets: ";
  List.iter (fun a -> Sc.fprint_set stdout (Sc.T.ft a com); Printf.printf " ") ls;
  Printf.printf "\nNew facet: ";
  Sc.fprint_set stdout real_new_ft;
  Printf.printf "\n";
  *)

  (*
  (* *) if output then
        ( Printf.printf "\t\t\t\tAdd project ";
          fprint_set stdout new_ft;
          Printf.printf "  [from %i joined " (List.length ls);
          fprint_set stdout all_nodes;
          Printf.printf "]\n";
        );
  *)

  let response =  mut_add_facet real_new_ft com in
  (ls, response) (* returns 
                    the list of facet ids that were sampled, and mut_add_facet response *)
let mut_add_project_acl_size_dist acl_var (alpha, cc, ll) fsize_ls com = 
  let size = 
    match Common.Sample.from_list_prob fsize_ls with
    | Some s -> s
    | None -> failwith "Evolve: mut_add_project_acl_size_dist: facet size distribution is empty"
  in

  (*
  (* average facet size *)
  let afs = 
    let sum, num =
      Sc.fold_facets (fun a _ (sum, num) -> 
        let size = Sc.facet_size a com in
        (sum + size, num + 1)
      ) com (0, 0)
    in
    float sum /. float num
  in
  *)

  (* average facet size from distribution *)
  let afs = 
      List.fold_left (fun acc (v, p) -> 
        acc +. (float v) *. p
      ) 0.0 fsize_ls
  in

  (* expectation of binomial distribution: n*p *)
  let expected_size = (afs *. float ll) *. (alpha /. float ll) in
  (* new cc *)
  let round x = (x +. 0.5) |> floor |> int_of_float in

  let alt_cc = max 1 (size - round expected_size) in
  
  (* variant 1 *)
  (*
  let alt_cc = ((1.0 -. alpha) *. float size) |> ceil |> round in
  *)

  (* variant 1 - fixed *)
  (*
  let noise = Random.float 1.0 -. 0.5 in
  let alt_cc = round( ceil((1.0 -. alpha) *. (float size +. noise)) ) in
  *)

  let cc = alt_cc in
  (* Printf.fprintf stderr "%i\n" cc; *)

  match acl_var with
  | 1 -> mut_add_project_with_extra_person (alpha, cc, ll) com 
  | 2 -> mut_add_project_with_extra_person_2 (alpha, cc, ll) com 
  | 3 -> mut_add_project_with_extra_person_3 (alpha, cc, ll) com 
(*| 4 -> mut_add_project_with_extra_person_2_size (alpha, cc, ll) size com *)
  | _ -> failwith "Evolve: mut_add_project_acl_size_dist: model not implemented"


