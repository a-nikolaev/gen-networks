
(* adjacentcy set graph *)

open Common

module S = Sc.S

type t = {ht:(int, S.t) Hashtbl.t; nodes: int array}

let of_sc sc = 
  let ht = Hashtbl.create (Sc.nodes_num sc) in
  let arr = Array.make (Sc.nodes_num sc) (-1) in
  ignore (
    Sc.fold_nodes (fun i nbrs index ->
        let adj_nodes_set = 
          S.fold (fun a acc ->
              let facet = Sc.T.ft a sc in
              S.union acc facet
            ) 
            nbrs S.empty
        in
        Hashtbl.replace ht i adj_nodes_set;
        arr.(index) <- i;
        (index + 1)
      )
      sc 0
  );
  { ht ; nodes = arr }

let of_sc_facets_as_nodes sc = 
  let ht = Hashtbl.create (Sc.facet_num sc) in
  let arr = Array.make (Sc.facet_num sc) (-1) in
  ignore (
    Sc.fold_facets (fun a facet index ->
        let adj_facets_set = 
          S.fold (fun i acc ->
              let nbrs = Sc.T.nd i sc in
              S.union acc nbrs
            ) 
            facet S.empty
        in
        Hashtbl.replace ht a adj_facets_set;
        arr.(index) <- a;
        (index + 1)
      )
      sc 0
  );
  { ht; nodes = arr }

let random_node asg =
  asg.nodes.(Random.int (Array.length asg.nodes))

(* get the set of neighbors for node i *)
let get_nbrs asg i =
  try
    Hashtbl.find asg.ht i
  with
  | Not_found -> failwith "Asg: get_nbrs: node not found"

let con_comps asg =
  let n = Array.length asg.nodes in
  let group = Hashtbl.create n in
  let cc_size = Hashtbl.create n in
  asg.nodes |> Array.iter (fun i -> Hashtbl.replace group i (-1));

  (* assuming: node is not in any cc yet *)
  let rec expand cc_index node ((acc_count, acc_ls) as acc) =
    Hashtbl.replace group node cc_index;
    let nbrs = Hashtbl.find asg.ht node in
    S.fold (fun nd acc -> 
        if Hashtbl.find group nd < 0 then
          expand cc_index nd acc
        else acc
      ) 
      nbrs 
      (acc_count+1, node::acc_ls)
  in
  
  let rec next_cc cc_index ls_nodes cc_acc =
    match ls_nodes with
    | hd :: tl when Hashtbl.find group hd < 0 ->
        (* count number of nodes and returns list of nodes in the cc of the node node *)
        let (cc_count, cc_nodes_ls) = expand cc_index hd (0, []) in
        next_cc (cc_index + 1) tl ((cc_index, cc_count, cc_nodes_ls) :: cc_acc)
    | hd :: tl ->
        next_cc cc_index tl cc_acc
    | [] -> cc_acc
  in

  let ls_initial = asg.nodes |> Array.to_list in
  let all_ccs = next_cc 0 ls_initial [] in
  (group, all_ccs)


let distances_from start_node asg =
  let n = Array.length asg.nodes in
  let group = Hashtbl.create n in
  asg.nodes |> Array.iter (fun i -> Hashtbl.replace group i (-1));

  let rec go_to_next_layer dist layer hist =
    if S.cardinal layer > 0 then
    ( 
      let nn = S.cardinal layer in
        
      S.iter (fun node ->
          Hashtbl.replace group node dist;
        )
        layer;
      
      let next_layer = 
        S.fold (fun node acc ->
            let nbrs = Hashtbl.find asg.ht node |> S.filter (fun nd -> Hashtbl.find group nd < 0) in
            S.union acc nbrs
          )
          layer S.empty
      in

      go_to_next_layer (dist+1) next_layer ((dist,nn)::hist)
    )
    else
      hist
  in

  let start_layer = S.of_list [start_node] in

  let hist = go_to_next_layer 0 start_layer [] in
  let disconnected = n - List.fold_left (fun acc (d,nn) -> acc + nn) 0 hist in
  
  ((-1, disconnected) :: List.rev hist)



let all_avg_lengths asg =

  let average hist =
    match hist with
    | [] -> 0.0
    | _ ->
      let sum, sum_prop = List.fold_left (fun (sum, sum_prop) (x, prop) -> (sum +. (float x) *. (float prop), sum_prop +. float prop)) (0.0, 0.0) hist in
      sum /. sum_prop
  in

  Printf.printf "\nGenerated. \n\n%!";

  (* find connected components *)
  let ccht, ccs = con_comps asg in
  
  Printf.printf "\nFound connected components. \n\n%!";
   
  let rec repeat_for_all nodes_ls i (len_sum, len_num) =
    match nodes_ls with
    | node::tl ->
        let hist = distances_from node asg in

        let len = 
          match hist with
          | (-1,_) :: (0, _) :: tl -> average tl 
          | _ -> failwith "Asg: all_avg_lengths: malformed distances histogram"
        in

        repeat_for_all tl (i+1) (len_sum +. len, len_num + 1)
    
    | [] -> 
        len_sum /. float len_num
  in

  (* find average length in each *)
  ccs |> List.map (fun (cc_index, count, nodes_ls) ->
    match count with
    | 1 -> (count, 0.0)
    | 2 -> (count, 1.0)
    | _ ->
      let avg_len = repeat_for_all nodes_ls 0 (0.0, 0) in
      (count, avg_len)
  )

let all_avg_lengths_fast asg =

  let average hist =
    match hist with
    | [] -> 0.0
    | _ ->
      let sum, sum_prop = List.fold_left (fun (sum, sum_prop) (x, prop) -> (sum +. (float x) *. (float prop), sum_prop +. float prop)) (0.0, 0.0) hist in
      sum /. sum_prop
  in

  Printf.printf "Generated. %!";

  (* find connected components *)
  let ccht, ccs = con_comps asg in
  
  Printf.printf "Found connected components. \n%!";
   
  let rec repeat_for_all nodes_ls i (len_sum, len_num) =
    match nodes_ls with
    | node::tl ->
        let hist = distances_from node asg in

        let len = 
          match hist with
          | (-1,_) :: (0, _) :: tl -> average tl 
          | _ -> failwith "Asg: all_avg_lengths: malformed distances histogram"
        in

        repeat_for_all tl (i+1) (len_sum +. len, len_num + 1)
    
    | [] -> 
        len_sum /. float len_num
  in

  (* find average length in each *)
  ccs |> List.map (fun (cc_index, count, nodes_ls) ->
    match count with
    | 1 -> (count, 0.0)
    | 2 -> (count, 1.0)
    | _ ->
      if count <= 2000 then
        let avg_len = repeat_for_all nodes_ls 0 (0.0, 0) in
        (count, avg_len)
      else
        let nodes_arr = Array.of_list nodes_ls in
        let smaller_nodes_ls = fold_up (fun _ acc -> nodes_arr.(Random.int(count))::acc) 0 1000 [] in
        let avg_len = repeat_for_all smaller_nodes_ls 0 (0.0, 0) in
        (count, avg_len)

  )


(* single-sourse distance search *)
let find_all_distances asg cc_size cc_nodes_ls starting_layer =
  
  let make_new_groups () = 
    let group = Hashtbl.create cc_size in
    cc_nodes_ls |> List.iter (fun i -> Hashtbl.replace group i (-1));
    group
  in
  
  (* classify network into groups *)
  let rec go_to_next_layer group dist layer =
    if not (S.is_empty layer) then
    ( 
      S.iter (fun node ->
          Hashtbl.replace group node dist;
        )
        layer;
      let next_layer = 
        S.fold (fun node acc ->
            let nbrs = Hashtbl.find asg.ht node |> S.filter (fun nd -> Hashtbl.find group nd < 0) in
            S.union acc nbrs
          )
          layer S.empty
      in
      go_to_next_layer group (dist+1) next_layer 
    )
  in
    
  let group = make_new_groups () in
  go_to_next_layer group 0 starting_layer;
  group


(* Weighted average distance computation *)

let wd_generate_sample_for_cc asg cc_size cc_nodes_ls s0 k =
  
  (* initial sampling coefficients *)
  let v_gamma = Hashtbl.create cc_size in
  begin
    let init_value = 1.0 /. float cc_size in
    cc_nodes_ls |> List.iter (fun i -> Hashtbl.replace v_gamma i init_value)
  end;

  (* update coefficients while going through the initial set of vertices s0 *)
  s0 |> S.iter (fun u ->

    let group = find_all_distances asg cc_size cc_nodes_ls (S.of_list [u]) in

    let sum_w = Hashtbl.fold (fun node dist acc -> acc + dist) group 0 in

    let factor = 1.0 /. float sum_w in

    group |> Hashtbl.iter (fun node dist ->
      let old = Hashtbl.find v_gamma node in 
      let candidate = float dist *. factor in
      Hashtbl.replace v_gamma node (max old candidate)
    )
  );

  (* make a sample as a list of (node, probability) *)
  Hashtbl.fold (fun node gamma acc ->
    let p = min 1.0 (k *. gamma) in
    if Random.float 1.0 < p then
      (node,p) :: acc  (* add v in the sample *)
    else
      acc           (* reject it *)
  ) v_gamma []


let wd_avg_lengths_for_cc asg cc_size cc_nodes_ls epsilon =
  let n = cc_size in
  let cc_nodes_arr = Array.of_list cc_nodes_ls in
  (* sample log(n) many nodes for s0 *)
  let s0 = 
    let num_to_sample = (log (float n)) |> ceil |> int_of_float in
    fold_up (fun _ acc -> S.add cc_nodes_arr.(Random.int(n)) acc) 0 num_to_sample S.empty
  in
  let k = (epsilon ** (-2.0)) *. log (float n) in

  (* get a sample list *)
  let s = wd_generate_sample_for_cc asg cc_size cc_nodes_ls s0 k in 
  
  (* sum of distances to other nodes for each node *)
  let v_sum_length = Hashtbl.create n in
  cc_nodes_ls |> List.iter (fun i -> Hashtbl.replace v_sum_length i 0.0);

  (* update coefficients while going through the initial set of vertices s0 *)
  s |> List.iter (fun (u, pu) ->

    let group = find_all_distances asg cc_size cc_nodes_ls (S.of_list [u]) in

    let factor = 1.0 /. pu in

    group |> Hashtbl.iter (fun node dist ->
      let old = Hashtbl.find v_sum_length node in 
      Hashtbl.replace v_sum_length node (old +. float dist *. factor)
    )
  );

  let sum, num = Hashtbl.fold (fun node sum_len (sum, num) -> (sum +. sum_len, num + 1)) v_sum_length (0.0, 0) in 
  if num > 0 && n > 1 then
    (sum /. float num) /. (float (n-1))
  else
    0.0

let wd_avg_lengths_for_cc_median_of_three asg cc_size cc_nodes_ls epsilon =
  let f() = wd_avg_lengths_for_cc asg cc_size cc_nodes_ls epsilon in
  let a = f() in
  let b = f() in
  let c = f() in
  let arr = [| a; b; c |] in
  Array.sort compare arr;
  arr.(1)

let wd_all_avg_lengths asg =

  let average hist =
    match hist with
    | [] -> 0.0
    | _ ->
      let sum, sum_prop = List.fold_left (fun (sum, sum_prop) (x, prop) -> (sum +. (float x) *. (float prop), sum_prop +. float prop)) (0.0, 0.0) hist in
      sum /. sum_prop
  in

  (*Printf.printf "\nGenerated. \n\n%!";*)
  
  (* exact average length *)
  let rec repeat_for_all nodes_ls i (len_sum, len_num) =
    match nodes_ls with
    | node::tl ->
        let hist = distances_from node asg in

        let len = 
          match hist with
          | (-1,_) :: (0, _) :: tl -> average tl 
          | _ -> failwith "Asg: all_avg_lengths: malformed distances histogram"
        in

        repeat_for_all tl (i+1) (len_sum +. len, len_num + 1)
    
    | [] -> 
        len_sum /. float len_num
  in


  (* find connected components *)
  let ccht, ccs = con_comps asg in
  
  (* Printf.printf "\nFound connected components. \n\n%!"; *)

  (* find average length in each *)
  ccs |> List.map (fun (cc_index, count, nodes_ls) ->
    match count with
    | 1 -> (count, 0.0)
    | 2 -> (count, 1.0)
    | _ ->
        let epsilon = 0.1 in
        let avg_len = wd_avg_lengths_for_cc asg count nodes_ls epsilon in
        (count, avg_len)
  )
