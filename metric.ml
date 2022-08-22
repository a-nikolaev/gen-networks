
open Sc

(*  Log M1 
 
    log ( Prod_v (1 + 1/dv)^dv ) = Sum_v ( dv log(1+1/dv) )           *)
let metric61 com =
  fold_nodes (fun i _ acc ->
    let d = float (facet_degree i com) in
    acc +. d *. log1p(1.0 /. d);
  ) com 0.0

(*  Log M2
 
    log ( Prod_f (1/|f|) Sum_{v in f} (1 + 1/dv)^|f| )  =  Sum_f ( log [ ( Sum_{v in f} (1+1/dv)^|f| ) / |f| ] )    *)

let metric62 com =
  fold_facets (fun a _ acc ->
    let sz = float (facet_size a com) in
    let sum = 
      fold_nodes_in_facet (fun i acc ->
        let d = float (facet_degree i com) in
        acc +. (1.0 +. 1.0 /. d) ** sz 
      )
      (com, a) 0.0
    in
    acc +. log (sum /. sz);
  ) com 0.0

(* Log M3
   
   log ( Prod_f ( 1 + 1 / (Sum_{v in f} dv) ) ^ (Sum_{v in f} dv) )
   = Sum_f (D * log (1+1/D)), where D = Sum_{v in f} dv                 *)

let metric63 com =
  fold_facets (fun a _ acc ->
    let degree_sum = fold_nodes_in_facet (fun i acc -> acc +. float (facet_degree i com)) (com, a) 0.0 in
    acc +. degree_sum *. log1p (1.0 /. degree_sum);
  ) com 0.0

(* M4
 
  Sum_f ( (|f| - 1) * [Prod_{v in f} 1/dv] )   *)

let metric54 com =
  fold_facets (fun a _ acc ->
    let sz = float (facet_size a com) in
    let prod = fold_nodes_in_facet (fun i acc -> acc /. float (facet_degree i com)) (com, a) 1.0 in
    acc +. (sz -. 1.0) *. prod;
  ) com 0.0


(* M5 
    
  Sum_f ( H(|f|) * [Sum_{v in f} 1/dv] )   *)

let harmonic =
  Memo.mem_rec (fun self n ->
    if n <= 0 then 
      0.0
    else
      self (n-1) +. 1.0 /. float n
  )

let metric55 com =
  fold_facets (fun a _ acc ->
    let sz = facet_size a com in
    let sum = fold_nodes_in_facet (fun i acc -> acc +. 1. /. float (facet_degree i com)) (com, a) 0.0 in
    acc +. harmonic sz *. sum;
  ) com 0.0


(* M6 *)

let metric56 com =
  fold_facets (fun a _ acc ->
    let sum = fold_nodes_in_facet (fun i acc -> acc +. 1. /. sqrt (float (facet_degree i com))) (com, a) 0.0 in
    acc +. (sum*.sum);
  ) com 0.0

(* M7 *)

let metric57 com =
  fold_facets (fun a _ acc ->
    let sum = fold_nodes_in_facet (fun i acc -> let d = float (facet_degree i com) in acc +. 1. /. (d*.d)) (com, a) 0.0 in
    acc +. sqrt sum;
  ) com 0.0

(* Log Factorial *)

let metric60 com =
  let v = 
    fold_facets (fun a _ acc ->
      let sz = float (facet_size a com) in
      acc +. Gamma.Lanczos.gamma (sz +. 1.0) 
    ) com 0.0
  in
  log v

(* M(com1) - M(com2) *)
let metric60_diff com1 com2 =
  let find_indexes com =
    Hashtbl.fold (fun a _ acc -> a::acc) com.T.ft [] |> List.sort compare
  in
  let indexes1 = find_indexes com1 in
  let indexes2 = find_indexes com2 in
  
  let fact n = Gamma.Lanczos.gamma (n +. 1.0) in
  
  let f com a =
    let sz = float (facet_size a com) in
    fact sz
  in

  let rec next acc xls yls = 
    match (xls, yls) with
    | x::xs, y::ys when x = y -> next (acc +. (f com1 x -. f com2 y)) xs ys
    | x::xs, y::ys when x < y -> next (acc +. (f com1 x -. 0.0))      xs yls
    | x::xs, y::ys ->            next (acc +. (0.0 -. f com2 y))      xls ys
    | x::xs, [] ->               next (acc +. (f com1 x -. 0.0))      xs []
    | [], y::ys ->               next (acc +. (0.0 -. f com2 y))      [] ys
    | [], [] ->                  acc
  in

  next 0.0 indexes1 indexes2

(* Quantitative metrics 
 
  - Max facet size     101
  - Avg facet size     102
  - Max facet degree   103
  - Avg facet degree   104
  - Max edge degree    105
  - Avg edge degree    106
  
  - Num of nodes       107
  - Num of facets      108
  - Num of ConComp     109
  
  - negative (Num of facets)
  - negative (Num of ConComp) 

  - negative (Sum of ConComp Diameters + Num of ConComp)
 
 *)

let max_facet_size com =
  fold_facets (fun a facet acc -> max acc (facet_size a com)) com 0

let avg_facet_size com = 
  let sum, num = fold_facets (fun a facet (s,n) -> (s + facet_size a com, n + 1)) com (0,0) in
  if num > 0 then float sum /. float num else 0.0

let max_facet_degree com =
  fold_nodes (fun i nbrs acc -> max acc (facet_degree i com)) com 0

let avg_facet_degree com = 
  let sum, num = fold_nodes (fun i nbrs (s,n) -> (s + facet_degree i com, n + 1)) com (0,0) in
  if num > 0 then float sum /. float num else 0.0

let max_edge_degree com =
  let n = nodes_num com in
  let arr = Array.make n false in
  fold_nodes (fun i nbrs acc -> 
      Array.fill arr 0 n false;
      S.iter (fun a -> 
          let id j = Hashtbl.find com.T.nd_sampler.Sampler.id j in
          S.iter (fun j -> if j <> i then arr.(id j) <- true ) (T.ft a com)
        ) nbrs;
      let edge_degree = Array.fold_left (fun acc v -> acc + if v then 1 else 0) 0 arr in
      max acc edge_degree 
    )
    com 0

let avg_edge_degree com =
  let n = nodes_num com in
  let arr = Array.make n false in
  let sum, num = 
    fold_nodes (fun i nbrs (sum, num) -> 
        Array.fill arr 0 n false;
        S.iter (fun a -> 
            let id j = Hashtbl.find com.T.nd_sampler.Sampler.id j in
            S.iter (fun j -> if i <> j then arr.(id j) <- true ) (T.ft a com)
          ) nbrs;
        let edge_degree = Array.fold_left (fun acc v -> acc + if v then 1 else 0) 0 arr in
        (sum + edge_degree, num + 1)
      )
      com (0, 0)
  in
  if num > 0 then float sum /. float num else 0.0

let num_concomp com =
  let n = nodes_num com in
  let arr = Array.make n 0 in
  let id i = Hashtbl.find com.T.nd_sampler.Sampler.id i in
  let node k = com.T.nd_sampler.Sampler.value.(k) in

  let rec next k cc_id =
    if k < n then
    ( if arr.(k) = 0 then
      ( let rec traverse k_cur count =
          if arr.(k_cur) = 0 then
          ( 
            arr.(k_cur) <- cc_id;
            let nbrs = T.nd (node k_cur) com in
            S.fold (fun a acc -> 
              S.fold (fun i acc ->
                traverse (id i) acc
              ) (T.ft a com) acc
            ) nbrs (count + 1)
          )
          else count
        in
        let concomp_size = traverse k 0 in
        next (k+1) (cc_id+1)
      )
      else next (k+1) cc_id
    )
    else (cc_id - 1)
  in
  let num = next 0 1 in
  num

let metric = 
  let mk_float f = (fun com -> float (f com)) in
  
  let compute = function
    | 61 -> metric61
    | 62 -> metric62
    | 63 -> metric63

    | 54 -> metric54
    | 55 -> metric55
    | 56 -> metric56
    | 57 -> metric57
    
    | 60 -> metric60

    | 101 -> mk_float  max_facet_size 
    | 102 -> avg_facet_size
    | 103 -> mk_float max_facet_degree
    | 104 -> avg_facet_degree
    | 105 -> mk_float max_edge_degree
    | 106 -> avg_edge_degree
    
    | 107 -> mk_float nodes_num
    | 108 -> mk_float facet_num
    | 109 -> mk_float num_concomp
    
    | x -> failwith ("metric: metric "^string_of_int x^" is undefined")
  in
      
  let compute2 m = 
    if m >= 1000 then
        (fun com -> com |> skeleton |> (compute (m mod 1000)))
    else
      compute m
  in

  ( fun x ->
      if x < 0 then
        (fun com -> -. (compute2 (-x) com))
      else
        compute2 x)


