
open Virt_graph

let pi = 4. *. atan 1.

let usage () =
  Printf.printf "Usage: \n\t./render_tree [0 | 1 | 2] virt-graph-file output.pdf\n\n"


let draw rendering_style gr output_file =

  let count_desc = Memo.mem_rec (fun self node ->
    let nbrs = Hashtbl.find gr.fw node in
    match nbrs with
    | [] -> 1
    | _ -> 
        List.fold_left (fun acc n -> self n + acc) 0 nbrs
  )
  in
  
  let node0 = 0 in

  let all_leaves = 
    let d = count_desc node0 in
    match Hashtbl.find gr.fw node0 with
    | [] | [_] ->
        d+1
    | _ -> d
  in

  let degree node =
    let nbrs_fw = Hashtbl.find gr.fw node in
    let nbrs_bk = Hashtbl.find gr.bk node in
    List.length nbrs_fw + List.length nbrs_bk 
  in

  let max_height_level, node_height = 
    let ht = Hashtbl.create 128 in
    
    let ht_remain = Hashtbl.create 128 in

    let node0_is_leaf =
      let nbrs_zero_fw = Hashtbl.find gr.fw node0 in
      match nbrs_zero_fw with
      | [] | [_] -> true
      | _ -> false
    in

    let get node = try Some (Hashtbl.find ht node) with Not_found -> None in
   
    let rec get_leaves acc node =
      let nbrs_fw = Hashtbl.find gr.fw node in
      let nbrs_bk = Hashtbl.find gr.bk node in
      let all_neighbors_num = List.length nbrs_fw + List.length nbrs_bk in
      Hashtbl.replace ht_remain node all_neighbors_num;

      match nbrs_fw with
      | [] ->
          node :: acc
      | ls ->
          List.fold_left (fun acc2 n -> get_leaves acc2 n) acc ls
    in

    let boundary = get_leaves (if node0_is_leaf then [node0] else []) node0 in
    
    let rec push_boundary lvl boundary =
      List.fold_left (fun next_boundary n ->
            
        Hashtbl.replace ht n lvl;
        Hashtbl.replace ht_remain n 0;
        
        let nbrs_fw = Hashtbl.find gr.fw n in
        let nbrs_bk = Hashtbl.find gr.bk n in

        let f ls acc =
          List.fold_left (fun acc n2 -> 
            let rem = Hashtbl.find ht_remain n2 in
            if rem > 0 then
            ( let new_rem = rem - 1 in
              Hashtbl.replace ht_remain n2 new_rem;
              if new_rem = 1 then
                n2::acc
              else 
                acc
            )
            else
              acc
              
          ) acc ls
        in

        next_boundary |> f nbrs_fw |> f nbrs_bk

      ) [] boundary;
    in

    let rec iterate level boundary =
      match boundary with
      | [] -> (level-1)
      | _ -> 
          iterate (level+1) (push_boundary level boundary)
    in

    let max_height_level = iterate 0 boundary in

    (max_height_level, (Hashtbl.find ht))
  in

  (* drawing *)
  let w = 2000.0 in
  let h = 2000.0 in
  
  let surface = Cairo.PDF.create output_file ~w:w ~h:h in
  let cr = Cairo.create surface in
  
  let rec plot level angle node =
    
    let nbrs = Hashtbl.find gr.fw node in
    let all_desc_num = count_desc node in

    let nbrs = 
      let f = (fun node -> count_desc node) in 
      let f _ = Random.int 100000 in
      nbrs |> List.sort (fun node1 node2 -> compare (f node1) (f node2))  
    in

    nbrs |> List.fold_left (fun (sum_before) child_node ->

      let child_node_desc_num = count_desc child_node in
      let left = child_node_desc_num in
      let right = all_leaves - child_node_desc_num in
      let sum = float left +. float right in
      let diff = abs_float (float left -. float right) in
      let smallness = diff /. sum in

      let d = 
        match rendering_style with 
        | 0 -> 
            (* 0 *)
            200.0 *. exp(-. float level *. 0.3)
        
        | 1 ->
            (* 1 *)
            200.0 *. exp(-. 2.1 *. smallness)

        | 2 ->
            (* 2 *)
            let phi = (1.0 +. sqrt(5.0)) *. 0.5 in
            let phi = phi ** 1.0 in
            let min_edge_len = 0.15 *. w /. (phi**(float max_height_level)) in
            let min_edge_len = 60.0 in
            min_edge_len *. (phi ** sqrt (float(max (node_height child_node) (node_height node))))
      in

      let dx = d in
      (* let dy = float sum_before *. d in *)
      let dy = 0.0 in

      let subangle = (angle /. float (all_desc_num)) *. (float child_node_desc_num) in
      let d_angle = -0.5*.angle +. (angle /. float (all_desc_num)) *. (float sum_before) +. 0.5 *. subangle  in


      Cairo.save cr;
      Cairo.rotate cr d_angle;
      Cairo.move_to cr 0.0 0.0;
      Cairo.line_to cr dx dy;
      Cairo.stroke cr;
      
      Cairo.translate cr dx dy;
      (* plot (level + 1) (subangle*.(1.0 +. 0.08 *. float level)) child_node; *)
      (* plot (level + 1) (subangle*.(1.0 +. 0.4 -. 0.35 *. smallness)) child_node;  *)
      plot (level + 1) (subangle*.(1.0 +. 0.25 -. 0.25 *. smallness)) child_node; 
      (* plot (level + 1) (subangle) child_node; *)
      Cairo.restore cr;

      sum_before + child_node_desc_num
    )
    0;
    
    if Hashtbl.find gr.rmvd_by node <> None then
      Cairo.set_source_rgba cr 0.5 0.5 1.0 1.0;

    let rad = 5.0 +. 1.5 *. float (degree node) in

    Cairo.arc cr 0.0 0.0 rad 0.0 (2.0*.pi);
    Cairo.fill cr;
    Cairo.set_source_rgba cr 0.0 0.0 0.0 1.0;
    

  in

  Cairo.translate cr (w*.0.5) (h*.0.5);
  
  Cairo.set_source_rgba cr 1.0 0.2 0.0 0.5;
  Cairo.arc cr 0.0 0.0 15.0 0.0 (2.0*.pi);
  Cairo.fill cr;
  Cairo.set_source_rgba cr 0.0 0.0 0.0 1.0;

  let initial_angle =
    let all_leaves = count_desc node0 in
    match (Hashtbl.find gr.fw node0) with
    | [] | [_] -> (2.0 *. pi) -. (2.0 *. pi) /. float (all_leaves+1)
    | _ -> (2.0 *. pi)
  in
  plot 0 initial_angle node0;
    
  Cairo.stroke cr;

  Cairo.Surface.finish surface


let () =

  if Array.length Sys.argv >= 4 then begin
    Random.self_init(); 
    let rendering_style = int_of_string Sys.argv.(1) in
    let gr = Virt_graph.load_bin Sys.argv.(2) in
    let output_file = Sys.argv.(3) in
    draw rendering_style gr output_file
  end else
    usage()
