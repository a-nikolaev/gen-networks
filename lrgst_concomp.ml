
let group_fast sc =
  let new_sc = Concomp.largest_cc sc in  
  Sc.fprint_normal stdout new_sc

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

  group_fast com



