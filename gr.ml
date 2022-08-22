

(** A directed graph model with possibly multi-edges and self-loops 
 * it is able to uniformly sample nodes and edges *)
type node = int
type edge = node * node

type dir_type = In | Out | Both
type nbhd_type = Open of dir_type | Closed of dir_type

module M = Map.Make (struct type t = node let compare = compare end)

type t = {
  (* structure *)

  (* out-links except self-loops *)
  out_maps : (node, ((int M.t) * int)) Hashtbl.t; (* node -> ((map of out-adjacent nodes -> multi-edge count), node out-cardinality) *)

  (* in-links except self-loops *)
  in_maps :  (node, ((int M.t) * int)) Hashtbl.t; (* node -> ((map of in-adjacent nodes  -> multi-edge count), node in-cardinality) *)

  (* self-loops *)
  mutable self_loops: (node, int) Hashtbl.t;

  (* sampling *)
  mutable num_nodes : int; 
  mutable num_edges : int;
  nodes : (int, node) Hashtbl.t; (* index -> node *)
  edges : (int, edge) Hashtbl.t  (* index -> edge *)
}

let make () = { 
  out_maps = Hashtbl.create 1 ;
  in_maps = Hashtbl.create 1 ;
  self_loops = Hashtbl.create 1;
  num_nodes = 0 ;
  num_edges = 0 ;
  nodes = Hashtbl.create 1 ;
  edges = Hashtbl.create 1 ;
}

(* Mumber of nodes *)

let num_nodes gr = gr.num_nodes

(* Mumber of edges *)

let num_edges gr = gr.num_edges

(* Degrees *)

let self_loops gr i =
  try Hashtbl.find gr.self_loops i with 
  | Not_found -> failwith "Gr: self_loops: node not found"

let degree =

  let op_out gr i = snd (Hashtbl.find gr.out_maps i) in
  let cl_out gr i = snd (Hashtbl.find gr.out_maps i) in
  let op_in gr i = op_out gr i + self_loops gr i  in
  let cl_in gr i = op_in gr i + self_loops gr i  in

  (fun nt gr i ->
    try 
      begin match nt with
      | Open Out -> op_out gr i
      | Open In ->  op_in gr i
      | Closed Out -> cl_out gr i
      | Closed In ->  cl_in gr i
      | Open Both -> op_out gr i + op_in gr i
      | Closed Both -> cl_out gr i + cl_in gr i
      end
    with Not_found -> failwith "Gr: degree: node not found"
  )

(* Neighborhoods *)

let nbhd =

  let op_out gr i = try fst (Hashtbl.find gr.out_maps i) with Not_found -> M.empty in
  let op_in gr i = try fst (Hashtbl.find gr.in_maps i) with Not_found -> M.empty in
  let cl_out gr i = (op_out gr i) |> M.add i (self_loops gr i) in
  let cl_in gr i = (op_in gr i) |> M.add i (self_loops gr i) in
  
  let union = M.union (fun _ v1 v2 -> Some (v1 + v2)) in
  
  (fun nt gr i ->     
    match nt with
    | Open Out -> op_out gr i
    | Open In ->  op_in gr i
    | Closed Out -> cl_out gr i
    | Closed In ->  cl_in gr i
    | Open Both -> union (op_out gr i) (op_in gr i)
    | Closed Both -> union (cl_out gr i) (cl_in gr i)
  )

(* Membership *)
  
let mem_node gr i =
  Hashtbl.mem gr.self_loops i

(* helper *)
let count_in_map key m = 
  try M.find key m with Not_found -> 0

let count_edge gr (i, j) =
  if not (mem_node gr i && mem_node gr j) then
    0
  else if i = j then
    self_loops gr i
  else
    if degree (Open Out) gr i < degree (Open In) gr j then
      count_in_map j (nbhd (Open Out) gr i)
    else
      count_in_map i (nbhd (Open In) gr j)

let mem_edge gr ij = count_edge gr ij > 0


(* Adding *)
  
exception Node_exists of node

exception Edge_exists of edge

let unsafe_add_node gr i =
  Hashtbl.replace gr.out_maps i (M.empty, 0);
  Hashtbl.replace gr.in_maps i (M.empty, 0);
  Hashtbl.replace gr.self_loops i 0;
  let index_i = gr.num_nodes in
  Hashtbl.replace gr.nodes index_i i;
  gr.num_nodes <- gr.num_nodes + 1

let add_node gr i =
  if mem_node gr i then
    raise (Node_exists i)
  else 
    unsafe_add_node gr i

let add_multi_edge gr (i, j) =
  if not (mem_node gr i) then unsafe_add_node gr i;
  if not (mem_node gr j) then unsafe_add_node gr j;

  if i = j then
    let v = Hashtbl.find gr.self_loops i in
    Hashtbl.replace gr.self_loops i (v+1)
  else begin
    (* add 1 degree to a map *)
    let add1 k (m, d) = 
      (M.add k (count_in_map k m + 1) m, d + 1)
    in
    
    let out_md = Hashtbl.find gr.out_maps i in
    Hashtbl.replace gr.out_maps i (add1 j out_md);
    
    let in_md = Hashtbl.find gr.in_maps j in
    Hashtbl.replace gr.in_maps j (add1 i in_md)
  end;

  let index_ij = gr.num_edges in
  Hashtbl.replace gr.edges index_ij (i,j);
  gr.num_edges <- gr.num_edges + 1

let add_unique_edge gr ij =
  if not (mem_edge gr ij) then
    add_multi_edge gr ij
  else
    raise (Edge_exists ij)

let try_add_unique_edge gr ij =
  if mem_edge gr ij then
    false
  else begin
    add_multi_edge gr ij;
    true
  end 

(* Sampling *)

let sample_node_uniform gr =
  Hashtbl.find gr.nodes (Random.int gr.num_nodes)

let sample_edge_uniform gr =
  Hashtbl.find gr.edges (Random.int gr.num_edges)


(* Makes a new node name. Assumes all nodes were added in order 0, 1, 2, ...  *)
let suggest_new_node gr = 
  gr.num_nodes


(* Seq *)
let to_seq_nodes gr = Hashtbl.to_seq_values gr.nodes

let to_seq_edges gr = Hashtbl.to_seq_values gr.edges


(* Export *)

(* Export to edge list (order not deterministic) 
 * Example: [(3,2); (3,0); (3,1); (2,2); (1,1); (1,0); (0,0)] *)
let to_edge_list gr =
  gr.edges |> Hashtbl.to_seq_values |> List.of_seq

let print_info gr =
  Printf.printf "nodes: %i\n" (num_nodes gr);
  Printf.printf "edges: %i\n" (num_edges gr);
  
  gr |> to_seq_nodes |> Seq.iter (fun i ->
    Printf.printf "%i: \n" i;
    
    Printf.printf "\t-> ";
    nbhd (Closed Out) gr i |> M.iter (fun j count -> 
      for x = 1 to count do
        Printf.printf "%i " j
      done;
    );
    Printf.printf "\n";

    Printf.printf "\t<- ";
    nbhd (Closed In) gr i |> M.iter (fun j count -> 
      for x = 1 to count do
        Printf.printf "%i " j
      done;
    );
    Printf.printf "\n\n"
  )

module Sample = struct

  let edge_uniform gr = sample_edge_uniform gr 

  let node_prop_closed_deg dir gr =
    let (i, j) = edge_uniform gr in
    match dir with
    | In -> j
    | Out -> i
    | Both -> if Random.int 2 = 0 then i else j

end


module Grow = struct

  (** One step of Price's growth model with out-degree m.

    Adds:
      - one new node 
      - up to m new outgoing edges from the new node 
      - a self-loop for the new node
    
    @return [(i, mm)] where 
     [i] is the name of the new node and 
     [m] is number of edges added not counting the self-loop

   *)
  let price_step gr m =
    let i = suggest_new_node gr in

    (* add node i *)
    add_node gr i;
    
    (* add self loop *)
    add_unique_edge gr (i, i);

    (* add m links to random nodes *)
    let rec repeat already_added tolerate_failures =
      if already_added < m && tolerate_failures >= 0 then
        let j = Sample.node_prop_closed_deg In gr in
        if try_add_unique_edge gr (i,j) then
          repeat (already_added + 1) tolerate_failures
        else
          repeat already_added (tolerate_failures - 1)
      else
        already_added
    in
   
    let mm = 
      if num_nodes gr <= 1 then 
        0 (* add only if there are other nodes *)
      else
        repeat 0 (m * 2) 
    in

    (i, mm)
  
  let lcd_step gr m =
    let i = suggest_new_node gr in
    (* add node i *)
    add_node gr i;

    (* add m links to random nodes *)
    for x = 1 to m do
      if Random.int (2 * num_edges gr + 1) = 0 then
        add_multi_edge gr (i,i)
      else
        let j = Sample.node_prop_closed_deg Both gr in
        add_multi_edge gr (i,j)
    done;
    (i, m)
  
end

module Cc = struct

  (* multiset operations *)
  let product = M.merge (fun _ v1 v2 -> 
    match v1, v2 with
    | Some a, Some b -> Some (a * b)
    | _ -> None
  )
  let inter = M.merge (fun _ v1 v2 -> 
    match v1, v2 with
    | Some a, Some b -> Some (min a b)
    | _ -> None
  )
  let union = M.merge (fun _ v1 v2 -> 
    match v1, v2 with
    | Some a, Some b -> Some (max a b)
    | None, _ -> v2
    | _, None -> v1
  )
  let sum = M.merge (fun _ v1 v2 -> 
    match v1, v2 with
    | Some a, Some b -> Some (a + b)
    | None, _ -> v2
    | _, None -> v1
  )
  let card m = M.fold (fun _ v acc -> acc + v ) m 0
 
  let flatten = M.map (fun _ -> 1)

  let diff m1 m2 =
    M.merge (fun _ oa ob ->
      match oa, ob with
      | Some a, Some b -> if a > b then Some (a-b) else None
      | Some a, None -> Some a
      | _ -> None
    ) m1 m2

  (* pair-multiset operations *)

  let pair_product nt gr i j =
    product (nbhd nt gr i) (nbhd nt gr j)
 
  let pair_inter nt gr i j =
    inter (nbhd nt gr i) (nbhd nt gr j)

  let pair_union nt gr i j = 
    union (nbhd nt gr i) (nbhd nt gr j)



  (* standard clusteing coefficient, reducing the multigraph to a simple graph (flatten) *)
  let gcc_standard gr = 
    
    let fn acc (i,j) =
      if i <> j then (* no self-loops *)
        acc +. ( (pair_product (Open Both) gr i j) |> flatten |> card |> float )
      else
        acc
    in
    let numer = gr |> to_seq_edges |> Seq.fold_left fn 0.0 in
    
    let fd acc i =
      let h acc j =
        if i > j then
          acc +. ( (pair_product (Open Both) gr i j) |> flatten |> card |> float )
        else
          acc
      in
      gr |> to_seq_nodes |> Seq.fold_left h acc
    in
    let denom = gr |> to_seq_nodes |> Seq.fold_left fd 0.0 in
    numer /. denom



  (* the same as the standard gcc, but without reducing it to a simple graph *)
  let gcc_multi gr = 
    
    let fn acc (i, j) =
      if i <> j then (* no self-loops *)
        acc +. ( (pair_product (Open Both) gr i j) |> card |> float )
      else
        acc
    in
    let numer = gr |> to_seq_edges |> Seq.fold_left fn 0.0 in
    
    let fd acc i =
      let h acc j =
        if i > j then
          acc +. ( (pair_product (Open Both) gr i j) |> card |> float )
        else
          acc
      in
      gr |> to_seq_nodes |> Seq.fold_left h acc
    in
    let denom = gr |> to_seq_nodes |> Seq.fold_left fd 0.0 in
    numer /. denom


  (* conversion *)
  let m_of_s s = Sc.S.fold (fun i acc -> M.add i 1 acc ) s M.empty 

  let m_of_node_nbhd i com =
    let nbhd = Sc.T.nd i com in
    M.empty
    |> Sc.S.fold (fun a acc -> sum acc @@ m_of_s (Sc.T.ft a com)) nbhd
    |> M.remove i

  let m_print m =
    M.iter (fun i count -> for x = 1 to count do Printf.printf "%i" i done; Printf.printf " ") m

  (* two-mode *)
  let gcc_multi_estrada com = 
    
    let numer = Sc.fold_facets (fun a ft acc ->

        (*
        Sc.fprint_set stdout ft;
        print_newline ();
        *)

        Sc.fold_nodes_in_facet (fun i acc -> 
          Sc.fold_nodes_in_facet (fun j acc -> 
            if (i > j) then
              let i_nbrs = Sc.T.nd i com |> Sc.S.remove a in
              let j_nbrs = Sc.T.nd j com |> Sc.S.remove a in

              let addition = 
                Sc.S.fold (fun b sub_acc -> 
                  Sc.S.fold (fun c sub_acc -> 
                    if b <> c then
                      let b_nodes = Sc.T.ft b com |> Sc.S.remove i |> Sc.S.remove j in
                      let c_nodes = Sc.T.ft c com |> Sc.S.remove i |> Sc.S.remove j in
                      sub_acc + ( product (m_of_s b_nodes) (m_of_s c_nodes) |> card )
                    else
                      sub_acc
                  ) j_nbrs sub_acc
                ) i_nbrs 0
              in
              acc +. float addition

            else
              acc
          ) (com, a) acc
        ) (com, a) acc
      ) com 0.0 
    in

    let denom = Sc.fold_nodes (fun i nbrs acc ->
        Sc.S.fold (fun a acc ->
          Sc.S.fold (fun b acc ->
            if a > b then
              let a_ft = Sc.T.ft a com |> Sc.S.remove i in
              let b_ft = Sc.T.ft b com |> Sc.S.remove i in

              let ab_inter = Sc.S.cardinal (Sc.S.inter a_ft b_ft) in
              let a_minus_b = Sc.S.cardinal (Sc.S.diff a_ft b_ft) in
              let b_minus_a = Sc.S.cardinal (Sc.S.diff b_ft a_ft) in
              
              acc +. 
                ( if ab_inter > 0 then
                    float (a_minus_b * b_minus_a + ab_inter * b_minus_a + a_minus_b * ab_inter + ab_inter * (ab_inter - 1)) 
                  else
                    float (a_minus_b * b_minus_a)
                )
            else
              acc
          ) nbrs acc 
        ) nbrs acc 
      ) com 0.0
    in

    (* Printf.printf "\n%g / %g = " numer denom; *)

    numer /. denom

  let gcc_multi_opsahl com = 

    let numer, denom = Sc.fold_nodes (fun i nbrs acc ->
        Sc.S.fold (fun a acc ->
          Sc.S.fold (fun b ((acc_num, acc_denom) as acc) ->
            if a > b then
              let a_ft = Sc.T.ft a com |> Sc.S.remove i in
              let b_ft = Sc.T.ft b com |> Sc.S.remove i in

              let ab_inter = Sc.S.cardinal (Sc.S.inter a_ft b_ft) in
              let a_minus_b = Sc.S.cardinal (Sc.S.diff a_ft b_ft) in
              let b_minus_a = Sc.S.cardinal (Sc.S.diff b_ft a_ft) in
              
              let acc_denom = 
                acc_denom +. 
                  if ab_inter > 0 then
                    float (a_minus_b * b_minus_a + ab_inter * b_minus_a + a_minus_b * ab_inter + ab_inter * (ab_inter - 1)) 
                  else
                    float (a_minus_b * b_minus_a) 
                  in
              
              let acc_num = 
                Sc.S.fold (fun j acc -> 
                  Sc.S.fold (fun k acc ->
                    if j <> k then
                      let j_fts = Sc.T.nd j com in
                      let k_fts = Sc.T.nd k com in
                      let num = if Sc.S.is_empty (Sc.S.inter j_fts k_fts |> Sc.S.remove a |> Sc.S.remove b) then 0.0 else 1.0 in
                      acc +. num
                    else
                      acc
                  ) b_ft acc
                ) a_ft acc_num
              in

              (acc_num, acc_denom)
            else
              acc
          ) nbrs acc 
        ) nbrs acc 
      ) com (0.0, 0.0)
    in

    (* Printf.printf "\n%g / %g = " numer denom; *)

    numer /. denom
end
