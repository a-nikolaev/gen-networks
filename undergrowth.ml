open Common
open Printf
open Sc
open Evolve

let make_unit_com obj_type =
  let com = make_empty obj_type in
  mut_add_facet (S.of_list [0]) com;
  com

type control = Any | Best of int

let report_graph com graph_fw graph_bk graph_kills graph_sup_by =
  let oc_gexf = open_out "./out-virt-graph/graph.gexf" in
  let nodes = Hashtbl.to_seq_keys graph_fw |> Array.of_seq in
  Array.sort compare nodes;

  Printf.fprintf oc_gexf 
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
 <gexf xmlns=\"http://www.gexf.net/1.2draft\" version=\"1.2\">
 <graph mode=\"static\" defaultedgetype=\"directed\">\n";

  Printf.fprintf oc_gexf "<nodes>\n";

  nodes |> Array.iter (fun i ->
    Printf.fprintf oc_gexf "  <node id=\"%i\" label=\"%i\" />\n" i i
  );
  
  Printf.fprintf oc_gexf "</nodes>\n";
  
  Printf.fprintf oc_gexf "<edges>\n";
 
  let edge_id = ref 0 in

  graph_fw |> Hashtbl.iter (fun i nbrs ->
    nbrs |> List.iter (fun j ->
      Printf.fprintf oc_gexf "  <edge id=\"%i\" source=\"%i\" target=\"%i\" />\n" (!edge_id) i j;
      edge_id := !edge_id + 1
    ) 
  );
  
  Printf.fprintf oc_gexf "</edges>\n";
  
  Printf.fprintf oc_gexf 
"</graph>
 </gexf>";

  close_out oc_gexf;
  

  let oc_dot = open_out "./out-virt-graph/graph.dot" in

  Printf.fprintf oc_dot "digraph{\n";

  nodes |> Array.iter (fun i ->
    Printf.fprintf oc_dot "  %i[shape=circle style=filled]\n" i
  );
  
  let edge_id = ref 0 in

  graph_fw |> Hashtbl.iter (fun i nbrs ->
    nbrs |> List.iter (fun j ->
      Printf.fprintf oc_dot "  %i -> %i \n" i j;
      edge_id := !edge_id + 1
    ) 
  );
  
  Printf.fprintf oc_dot "}";

  close_out oc_dot;


  let oc_data = open_out_bin "./out-virt-graph/graph.data" in
  Marshal.to_channel oc_data (com, graph_fw, graph_bk, graph_kills, graph_sup_by) [];
  close_out oc_data


(* returns number of iterations (i.e. how many mutations have happened) and the resulting complex *)
let grow control model (* (acl_var, (alpha, cc, ll)) *) stopping_rule step_callback opt_virt_graph com =

  let count_accidental = ref 0 in

  let mut_mutate =
      (fun i com ->
          let age = i+1 in

          let selected_ft_ls, outcome =
            match model with
            | Evolve.ACL (acl_var, (alpha, cc, ll)) ->
                begin match acl_var with
                | 1 -> Evolve.mut_add_project_with_extra_person (alpha, cc, ll) com 
                | 2 -> Evolve.mut_add_project_with_extra_person_2 (alpha, cc, ll) com 
                | 3 -> Evolve.mut_add_project_with_extra_person_3 (alpha, cc, ll) com 
                | _ -> failwith "Undergrowth: grow: mut_mutate: model not implemented"
                end
            | Evolve.ACdistL (acl_var, (alpha, cdist, ll)) ->
                let cc = 
                  match Sample.from_list_prob cdist with
                  | Some cc -> cc
                  | None -> failwith "Undergrowth: grow: mut_mutate: ACdistL empty distribution"
                in 
                begin match acl_var with
                | 1 -> Evolve.mut_add_project_with_extra_person (alpha, cc, ll) com 
                | 2 -> Evolve.mut_add_project_with_extra_person_2 (alpha, cc, ll) com 
                | 3 -> Evolve.mut_add_project_with_extra_person_3 (alpha, cc, ll) com 
                | _ -> failwith "Undergrowth: grow: mut_mutate: model not implemented"
                end
            | Evolve.SizeDist (fpn, fsize_ls) ->
                Evolve.mut_add_project_basu (fpn, fsize_ls) com 
            | Evolve.ACLSizeDist (acl_var, (alpha, cc, ll), fsize_ls) ->
                Evolve.mut_add_project_acl_size_dist acl_var (alpha, cc, ll) fsize_ls com 
          in

          (* accidental absorptions *)
          (*
          begin match outcome with
          | AddedWithDeletion (a, deleted_ls) ->
              let accidental_ls = deleted_ls |> List.filter (fun b -> not (List.mem b selected_ft_ls)) in
              let num = List.length accidental_ls in
              if num > 0 then begin
                count_accidental := !count_accidental + num;
                Printf.fprintf stderr "%i %i\n%!" i (!count_accidental)
              end
          | _ -> ()
          end;
          *)
          
          match opt_virt_graph with
          | None -> 
              ()
          | Some (gr, _) -> 
              let a, kills =  
                match outcome with
                | NotAdded _ -> failwith "Undergrowth: grow: mut_mutate: impossible to not add a facet"
                | AddedWithDeletion (a, deleted_ft_ls) -> (a, deleted_ft_ls)
                | Added a -> (a, [])
              in

              let new_ft = Sc.T.ft a com in
              
              let connected_to =
                selected_ft_ls 
                |> List.filter (fun aa ->
                    try 
                      let ft = Sc.T.ft aa com in
                      Sc.S.cardinal (Sc.S.inter ft new_ft) > 0
                    with
                      Not_found -> true
                  )
              in

              (* add new virt-node to virtual graph *)
              Virt_graph.add_singleton_node gr a;

              (* make old virt-nodes connect to it *)
              connected_to |> List.iter (fun old_a -> Virt_graph.add_directed_edge gr old_a a);

              begin match com.T.obj_type with
              | SimplicialComplex -> (* all absorbed facets get superseded in SC*)
                  kills |> List.iter (fun old_a -> Virt_graph.absorb gr a old_a )
              | Hypergraph -> () (* but not in hypergraph *)
              end
        (*
        match Random.int 4 with

        | _ ->


        | 0  -> 
            (* Printf.fprintf stderr "add-person\n"; *)
            Evolve.mut_add_person com
        | 1  -> 
            (* Printf.fprintf stderr "add-team\n"; *)
            Evolve.mut_add_project sampling com
        | 2 | _  -> 
            (* Printf.fprintf stderr "split-team\n"; *)
            Evolve.mut_split_project com
        *)
      )
  in  

  match control with
  | Best m -> 
      let rec evolve i com =
        if not (stopping_rule i com) then
        ( 
          let ncom1 = Sc.copy com in
          let ncom2 = Sc.copy com in
          let ncom3 = Sc.copy com in
       
          
          let output = true in
          
          mut_mutate i ncom1;
          mut_mutate i ncom2;
          mut_mutate i ncom3;
          
          let v1, v2, v3 = 
            let f = 
              if m = 60 then
                (fun new_com -> Metric.metric60_diff new_com com)
              else
                Metric.metric m 
            in
            (f ncom1, f ncom2, f ncom3)
          in 

          let ibest, ncom_best = if v1 > v2 && v1 > v3 then (1,ncom1) else if v2 > v3 then (2,ncom2) else (3,ncom3) in

          step_callback i ncom_best;

          evolve (i+1) ncom_best )
        else 
          (i, com)
      in
      evolve 0 com
  | Any -> 

      let filter_nodes size com =
        Sc.fold_facets (fun _ ft acc -> if Sc.S.cardinal ft = size then Sc.S.union ft acc else acc ) com Sc.S.empty
      in

      let report_fd_distrib select_size com =
        let dd = 10 in
        let arr = Array.make dd 0 in

        let nodes = match select_size with
        | Some size -> filter_nodes size com 
        | None -> com.Sc.T.nodes
        in

        Sc.S.iter (fun n ->
              let deg = Sc.facet_degree n com in
              if deg < dd then 
                arr.(deg) <- arr.(deg) + 1
          ) nodes;

        let all_count = Sc.S.cardinal nodes in 
        Array.iteri (fun d count -> 
            Printf.printf "%f " (float count /. float all_count)
        ) arr;

        Printf.printf "\n%!"
      in  

      let rec mut_evolve i com =
        
        if not (stopping_rule i com) then
        ( mut_mutate i com;
          step_callback i com;

          mut_evolve (i+1) com )
        else 
          (i, com)
      in
      
      let result = mut_evolve 0 com in

      result


