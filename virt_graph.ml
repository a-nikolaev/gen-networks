
type table = (int, int list) Hashtbl.t 
  
let table_add_empty ht k =
  Hashtbl.replace ht k []

let table_append ht k v =
  let prev = try Hashtbl.find ht k with Not_found -> [] in
  Hashtbl.replace ht k (v::prev)

type virt_graph =
  { fw : table;
    bk : table;
    rmvs : table;
    rmvd_by : (int, int option) Hashtbl.t 
  }

let make_empty () =
  { fw = Hashtbl.create 8;
    bk = Hashtbl.create 8;
    rmvs = Hashtbl.create 8;
    rmvd_by = Hashtbl.create 8;
  }

let add_singleton_node gr a =
  table_add_empty gr.fw a;
  table_add_empty gr.bk a;
  table_add_empty gr.rmvs a;
  Hashtbl.replace gr.rmvd_by a None

(* add directed edge a -> b *)
let add_directed_edge gr a b =
  table_append gr.fw a b; 
  table_append gr.bk b a

(* [a] absorbs [b] 
   So [b] is added in the list of nodes [a] rmvs, and [Some a] is assigned to [b] rmvd_by.
   Expected that [a] supersedes [b] and so [b] will never gain new edgess or connect to anything,
   although this data structure does not enforce this invariant
 *)
let absorb gr a b =
  table_append gr.rmvs a b;
  Hashtbl.replace gr.rmvd_by b (Some a)


(* serialization *)

let save_bin gr filename =
  let oc = open_out_bin filename in
  Marshal.to_channel oc gr [];
  close_out oc

let load_bin filename =
  let ic = open_in_bin filename in
  let gr = Marshal.from_channel ic in
  close_in ic;
  gr


(* reporting *)

let report_dot gr filename =
  let oc = open_out filename in
  let nodes = Hashtbl.to_seq_keys gr.fw |> Array.of_seq in
  Array.sort compare nodes;

  Printf.fprintf oc "digraph{\n";

  nodes |> Array.iter (fun i ->
    Printf.fprintf oc "  %i[shape=circle style=filled]\n" i
  );
  
  let edge_id = ref 0 in

  gr.fw |> Hashtbl.iter (fun i nbrs ->
    nbrs |> List.iter (fun j ->
      Printf.fprintf oc "  %i -> %i \n" i j;
      edge_id := !edge_id + 1
    ) 
  );
 
  Printf.fprintf oc "}";
  close_out oc


(* WARNING! the following function reporting the degrees of the virtual graph wasn't tested. *)
let report_degrees gr filename =
  let oc = open_out filename in
  let nodes_seq = Hashtbl.to_seq_keys gr.fw in
  nodes_seq |> Seq.iter (fun i ->
    let fw_deg = List.length(Hashtbl.find gr.fw i) in
    let bk_deg = List.length(Hashtbl.find gr.bk i) in
    Printf.printf "%i %i %i\n" bk_deg fw_deg (bk_deg + fw_deg)
  );
  close_out oc


let report_gexf com gr filename =
  let oc = open_out "./out-virt-graph/graph.gexf" in
  let nodes = Hashtbl.to_seq_keys gr.fw |> Array.of_seq in
  Array.sort compare nodes;

  Printf.fprintf oc 
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
 <gexf xmlns=\"http://www.gexf.net/1.2draft\" version=\"1.2\">
 <graph mode=\"static\" defaultedgetype=\"directed\">\n";

  Printf.fprintf oc "<nodes>\n";

  nodes |> Array.iter (fun i ->
    Printf.fprintf oc "  <node id=\"%i\" label=\"%i\" />\n" i i
  );
  
  Printf.fprintf oc "</nodes>\n";
  
  Printf.fprintf oc "<edges>\n";
 
  let edge_id = ref 0 in

  gr.fw |> Hashtbl.iter (fun i nbrs ->
    nbrs |> List.iter (fun j ->
      Printf.fprintf oc "  <edge id=\"%i\" source=\"%i\" target=\"%i\" />\n" (!edge_id) i j;
      edge_id := !edge_id + 1
    ) 
  );
  
  Printf.fprintf oc "</edges>\n";
  
  Printf.fprintf oc 
"</graph>
 </gexf>";

  close_out oc
