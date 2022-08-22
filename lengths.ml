
let _ =

  Random.self_init();
  
  let open Opt in
  let flag, param, _ =
    Opt.scan 
      (* flags  *) [Long "safe"] 
      (* params *) []
      Sys.argv 1
  in
  
  let safe = flag (Long "safe") in

  let com = Sc.parse_from_chan safe stdin in

  let asg = Asg.of_sc com in
  let lengths = Asg.wd_all_avg_lengths asg in
  let lengths_sorted = lengths |> List.rev |> List.sort (fun (x,_) (y,_) -> compare y x) in

  List.iter (fun (size, len) -> Printf.printf "%i %g\n" size len) lengths_sorted




