
open Printf
open Sc

let _ = 
  
  let open Opt in
  let flag, param, _ =
    Opt.scan 
      (* flags  *) [Long "safe";
                    Long "mfs"; Long "afs";
                    Long "mfd"; Long "afd";
                    Long "med"; Long "aed";
                    Long "nn";  Long "nf";
                    Long "ncc"
                   ] 
      (* params *) []
      Sys.argv 1
  in
  
  let safe = flag (Long "safe") in
  
  let com = parse_from_chan safe stdin in

  let ms = [101; 102; 103; 104; 105; 106; 107; 108; 109] in
  let lbls = ["MFS"; "AFS"; "MFD"; "AFD"; "MED"; "AED"; "NN"; "NF"; "NCC"] in
  
  (* is any label explicitly requested? *)
  let any_lbl_flag =
    lbls |> List.map String.lowercase_ascii |> List.exists (fun lwr_lbl -> flag (Long lwr_lbl))
  in

(*  printf "\n-------------------\n\n"; *)

  List.iter2 (fun m lbl ->
    let lowercase_lbl = String.lowercase_ascii lbl in

    if (not any_lbl_flag) || flag (Long lowercase_lbl) then begin
      let v = Metric.metric m com in
      Printf.printf "%g \t %s\n" v lbl
    end
  ) ms lbls;
(*  printf "\n-------------------\n" *)
