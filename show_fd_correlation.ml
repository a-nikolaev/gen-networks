
open Common

let binom =
  Memo.mem_rec (fun self (n,k) ->
    if n < 0 || k < 0 || k > n then 0
    else if k = 0 || k = n then 1
    else self (n-1, k-1) + self (n-1, k)
  )

let means ht_pair =
  let sum1, sum2, total = 
    Hashtbl.fold (fun (d1,d2) count (sum1, sum2, tot) ->
        let sum1 = sum1 +. float count *. float d1 in
        let sum2 = sum2 +. float count *. float d2 in
        let tot = tot +. float count in
        (sum1, sum2, tot)
    ) ht_pair (0.0, 0.0, 0.0)
  in
  (sum1 /. total, sum2 /. total)

let cov_sample ht_pair =
  let mean1, mean2 = means ht_pair in
  let sum, total = 
    Hashtbl.fold (fun (d1,d2) count (sum, tot) ->
        let sum2 = sum +. (float count) *. (float d1 -. mean1) *. (float d2 -. mean2) in
        let tot2 = tot +. float count in
        (sum2, tot2)
    ) ht_pair (0.0, 0.0)
  in
  sum /. (total -. 1.0)

let corr_sample ht_pair =
  let mean1, mean2 = means ht_pair in
  (* sigma squared *)
  let sum1, sum2, total = 
    Hashtbl.fold (fun (d1,d2) count (sum1, sum2, tot) ->
        let sum1 = sum1 +. float count *. (float d1 -. mean1)**2.0 in
        let sum2 = sum2 +. float count *. (float d2 -. mean2)**2.0 in
        let tot = tot +. float count in
        (sum1, sum2, tot)
    ) ht_pair (0.0, 0.0, 0.0)
  in
  let stddev_sq_1 = sum1 /. (total -. 1.0) in
  let stddev_sq_2 = sum2 /. (total -. 1.0) in
 
  (*
  Printf.printf "%g %g \n\n" stddev_sq_1 stddev_sq_2;
  Printf.printf "E[k1] E[k2] = %g \n\n" (mean1 *. mean1);
  *)

  let prod, total = 
    Hashtbl.fold (fun (d1,d2) count (prod, tot) ->
        let prod = prod +. float count *. (float d1 *. float d2) in
        let tot = tot +. float count in
        (prod, tot)
    ) ht_pair (0.0, 0.0)
  in

  (*
  Printf.printf "E[k1*k2] = %g \n\n" (prod /. total);
  *)

  (cov_sample ht_pair) /. sqrt (stddev_sq_1 *. stddev_sq_2)


let marginals ht_pair =
  let ht1 = Hashtbl.create 1 in
  let ht2 = Hashtbl.create 1 in

  let add ht key dv =
    let v = try Hashtbl.find ht key with Not_found -> 0 in
    Hashtbl.replace ht key (v + dv)
  in

  let total =
    Hashtbl.fold (fun (d1, d2) count acc -> 
      add ht1 d1 count;
      add ht2 d2 count;
      (acc + count)
    ) ht_pair 0 
  in

  (total, ht1, ht2)

let chi_squared_indep ht_pair = 
  let (total, ht1, ht2) = marginals ht_pair in
    
  let f_total = float total in

  let get ht key = 
    try Hashtbl.find ht key with Not_found -> 0 
  in
  
  let p1 d = float (get ht1 d) /. f_total in
  let p2 d = float (get ht2 d) /. f_total in

  let find_min_max_key ht = 
    Hashtbl.fold (fun key _ (lo, hi) -> (min key lo, max key hi)) ht (10000000000, -10000000000)
  in

  let min_d1, max_d1 = find_min_max_key ht1 in
  let min_d2, max_d2 = find_min_max_key ht2 in

  let statistic = 
    Seq.fold_left (fun acc d1 ->
      Seq.fold_left (fun acc d2 ->

        let observed = float (get ht_pair (d1, d2)) in
        let expected = (p1 d1) *. (p2 d2) *. f_total in

        let v =
          if expected > 0.0 then          
            (observed -. expected) ** 2.0 /. expected 
          else 
            0.0
        in

        acc +. v

      ) acc (min_d1 ==> (max_d1+1)) 
    ) 0.0 (min_d2 ==> (max_d2+1)) 
  in 

  let df = (max_d1 - min_d1) * (max_d2 - min_d2) in

  (statistic, df)


let show (s2, s11, a2, a11, ar2, ar11) sizes_ls = 
  
  Printf.printf   "# Same facet aggr\n";
    
  Printf.printf   "         ";
  Printf.printf   "PAIR   cov %15g   corr %15g " (cov_sample a2) (corr_sample a2);
  Printf.printf   "         ";
  Printf.printf   "ONES   cov %15g   corr %15g \n" (cov_sample a11) (corr_sample a11);
  
  Printf.printf   "# Random (i,j)\n";
  
  Printf.printf   "         ";
  Printf.printf   "PAIR   cov %15g   corr %15g " (cov_sample ar2) (corr_sample ar2);
  Printf.printf   "         ";
  Printf.printf   "ONES   cov %15g   corr %15g \n" (cov_sample ar11) (corr_sample ar11);

  Printf.printf   "# Conditioned\n";

  sizes_ls |> List.iter (fun size ->
    Printf.printf "size %3i " size;
    Printf.printf "PAIR   cov %15g   corr %15g " (cov_sample s2.(size)) (corr_sample s2.(size));
  Printf.printf   "         ";
    Printf.printf "ONES   cov %15g   corr %15g \n" (cov_sample s11.(size)) (corr_sample s11.(size));

    (*
    for di = 1 to 10 do
      for dj = 1 to 10 do
        let p = p_pair_cond data size (di, dj) in
        Printf.printf "%g\t" p;
      done;
      Printf.printf "\n"
    done;
   
    Printf.printf "---\n";

    for di = 1 to 10 do
      let pi = p1_cond data size di in
      for dj = 1 to 10 do
        let pj = p2_cond data size dj in
        let p = pi *. pj in
        Printf.printf "%g\t" p;
      done;
      Printf.printf "\n"
    done
    *)
  )

let indep_test ht_pair =
  
  let (total, ht1, ht2) = marginals ht_pair in

  let get ht key = 
    try Hashtbl.find ht key with Not_found -> 0 
  in

  Printf.printf "Total = %i\n" total;
 
  let stat, df = chi_squared_indep ht_pair in

  let normal = (stat -. float df) /. sqrt (2.0 *. float df) in
  Printf.printf "Statistic = %g\t df = %i\t (stat-df)/sqrt(2df) = %g\n" stat df normal;
  Printf.printf "cov = %g\t corr = %g\n\n" (cov_sample ht_pair) (corr_sample ht_pair);

  for di = 0 to 14 do
    for dj = 0 to 14 do
      let c = get ht_pair (di, dj) in
      Printf.printf "%10i" c;
    done;
    Printf.printf "\n"
  done;
 
  Printf.printf "---\n";

  for di = 0 to 14 do
    let ci = get ht1 di in
    for dj = 0 to 14 do
      let cj = get ht2 dj in
      let c = float (ci * cj) /. (float total) in
      Printf.printf "%10i" (round c);
    done;
    Printf.printf "\n%!"
  done

(** log-binning of a hash table ((d1, d2) -> count) *)
let log_bin_table ht_pair =
  
  let add ht key dv =
    let v = try Hashtbl.find ht key with Not_found -> 0 in
    Hashtbl.replace ht key (v + dv)
  in

  let bin x = (log (float x) /. log 2.0) |> floor |> int_of_float in

  let binned = Hashtbl.create 10 in
  
  Hashtbl.iter (fun (d1, d2) count ->
    if  2 <= d1 && d1 <= 31 && 2 <= d2 && d2 <= 31 then
      add binned (bin d1, bin d2) count
  ) ht_pair;

  binned

  

let () =

  let open Opt in
  let flag, param, _ =
    Opt.scan 
      (* flags  *) [] 
      (* params *) [Long "i"; Long "s"]
      Sys.argv 1
  in

  match param (Long "i"), param (Long "s") with
  | Some (Opt.OK filename), Some (Opt.OK sizes_str) ->
      let ic = open_in_bin filename in
      let data = Marshal.from_channel ic in
      close_in ic;

      let sizes_ls = 
        if sizes_str = "all" then
          let s2, _, _, _, _, _  = data in
          let max_size = Array.length s2 - 1 in
          fold_up (fun i acc -> i :: acc) 1 (max_size+1) [] |> List.rev
        else
          String.split_on_char ',' sizes_str |> List.map int_of_string
      in

      (* show data sizes_ls *)
      let s2, _, a2, _, _, _  = data in

      (* All facets *)
      Printf.printf "\nAll\n";
      indep_test (log_bin_table a2);
      
      (* Conditioned on size *)
      sizes_ls |> List.iter (fun size -> 
        Printf.printf "\nsize = %i\n" size;
        indep_test (log_bin_table s2.(size))
      )

  | _ ->
      Printf.printf "Usage:\n\t ./show_fd_correlation --i=corrfile --s=[1,2,3,4,... | all] \n\n"


