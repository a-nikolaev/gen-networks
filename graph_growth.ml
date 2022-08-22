
let grow out_deg n =
  
  let gr = Gr.make () in
  for i = 1 to n do
    ignore (Gr.Grow.lcd_step gr out_deg : (Gr.node * int))
  done;

  (Gr.to_edge_list gr) |> List.iter (fun (i,j) ->
    Printf.printf "%i -> %i\n" i j
  );

  let cc = Gr.Cc.gcc_standard gr in
  Printf.printf "\ncc = %g\n\n" cc ;

  let cc = Gr.Cc.gcc_multi gr in
  Printf.printf "\ncc = %g\n\n" cc 

let () =
  Random.self_init();
  grow 3 (int_of_string Sys.argv.(1))
