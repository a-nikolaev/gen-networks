
open Sc

let _ = 
  let safe = true in
  let com = parse_from_chan safe stdin in
  Sc.fprint_normal stdout com
