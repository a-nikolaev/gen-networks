

(* (a ==> b) is a sequence ranging over the interval a <= x < b *)

let rec (==>) a b () =
  if a < b then Seq.Cons (a, a+1 ==> b) else Seq.Nil

let rec seq_find_first pred seq = 
  match seq () with
  | Seq.Cons (x, rest) -> if pred x then Some x else seq_find_first pred rest
  | Seq.Nil -> None

let round x = (x +. 0.5) |> floor |> int_of_float

let rec fold_up f x xtop acc = if x < xtop then fold_up f (x+1) xtop (f x acc) else acc

module Sample = struct
  let from_list_opt ls =
    let len = List.length ls in
    if len > 0 then
      Some (List.nth ls (Random.int len))
    else
      None

  let from_list_prob ls = 
    let sum = List.fold_left (fun acc (_, p) -> acc +. p) 0.0 ls in
    let rec next x ls = 
      match ls with
      | (v, _) :: [] -> Some v
      | (_, p) :: tl when x > p -> next (x -. p) tl
      | (v, _) :: _ -> Some v
      | [] -> None
    in
    next (Random.float sum) ls
end


(* Gamma function *)

module Lanczos = struct
  let e = exp 1.
  let pi = 4. *. atan 1.
  let sqrttwopi = sqrt (2. *. pi)
  
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
