
let () =

  let open Opt in
  let flag, param, _ =
    Opt.scan 
      (* flags  *) [Long "safe"] 
      (* params *) []
      Sys.argv 1
  in
  
  let safe = flag (Long "safe") in

  let com = Sc.parse_from_chan safe stdin in

  let cc = Gr.Cc.gcc_multi_opsahl com in
  Printf.printf "%g\n" cc
 
  (*
  let cc2 = Gr.Cc.gcc_multi_estrada com in
  Printf.printf "%g\n" cc2
  *)
