
(* gamma function
  
  Gamma(n) = (n-1)!

  n! = Gamma(n+1)

 *)

let e = exp 1.
let pi = 4. *. atan 1.
let sqrttwopi = sqrt (2. *. pi)
 
module Lanczos = struct
  (* Lanczos method *)
  (* Coefficients used by the GNU Scientific Library *)
  let g = 7.
  let c = [|0.99999999999980993; 676.5203681218851; -1259.1392167224028;
	    771.32342877765313; -176.61502916214059; 12.507343278686905;
	    -0.13857109526572012; 9.9843695780195716e-6; 1.5056327351493116e-7|]
 
  let rec ag z d =
    if d = 0 then c.(0) +. ag z 1
    else if d < 8 then c.(d) /. (z +. float d) +. ag z (succ d)
    else c.(d) /. (z +. float d)
 
  let gamma z =
    let z = z -. 1. in
    let p = z +. g +. 0.5 in
    sqrttwopi *. p ** (z +. 0.5) *. exp (-. p) *. ag z 0
end
