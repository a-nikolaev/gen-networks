
(* ./virt_graph_degrees virt.bin indeg.fd outdeg.fd *)
  
let output gr filename_indeg filename_outdeg =
  
  let nodes = Hashtbl.to_seq_keys gr.Virt_graph.fw |> Array.of_seq in
  Array.sort compare nodes;

  let oc_in = open_out filename_indeg in
  let oc_out = open_out filename_outdeg in
  nodes |> Array.iter (fun i ->
    let indeg = try Hashtbl.find gr.Virt_graph.bk i |> List.length with Not_found -> 0 in
    let outdeg = try Hashtbl.find gr.Virt_graph.fw i |> List.length with Not_found -> 0 in

    Printf.fprintf oc_in "%i\n" indeg;
    Printf.fprintf oc_out "%i\n" outdeg
  );
 
  close_out oc_in;
  close_out oc_out

let () =

  if Array.length Sys.argv >= 4 then begin
    Random.self_init(); 
    let gr = Virt_graph.load_bin Sys.argv.(1) in
    let output_file_indeg = Sys.argv.(2) in
    let output_file_outdeg = Sys.argv.(3) in

    output gr output_file_indeg output_file_outdeg
  end
