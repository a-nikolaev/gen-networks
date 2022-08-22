
let make_du () =
  Union_find.empty ()

(* Disjoint union of facets *)
let du_of_sc sc =
  (* joining connected components *)
  let du = Union_find.empty () in

  (* add each facet index as an element in disjoint union *)
  Sc.fold_facets (fun a _ acc ->
      Union_find.add du a
    ) sc ();
  
  Sc.fold_nodes (fun i nbrs () ->
    match Sc.S.choose_opt nbrs with
    | Some a ->
        nbrs |> Sc.S.iter (fun b ->
          Union_find.union du a b
        )
    | None -> ()
  ) sc ();
  du

(* Number of facets in the largest connected component *)
let largest_cc_size sc =
  let du = du_of_sc sc in
  let sizes = Union_find.all_sizes du in
  List.fold_left max 0 sizes 

(* Make largest connected component subcomplex of sc *)
let largest_cc sc = 
  let du = du_of_sc sc in
  let roots = Union_find.all_roots du in
  let maxcc_root, _ = 
    match roots with
    | [] -> exit 1
    | a :: tl ->
        let a_size = Union_find.size du a in
        List.fold_left (fun (best, best_size) b ->
          let b_size = Union_find.size du b in
          if b_size > best_size then
            (b, b_size)
          else
            (best, best_size)
        ) (a, a_size) tl 
  in

  let new_sc = Sc.make_empty (sc.Sc.T.obj_type) in
  Sc.fold_facets (fun a facet () ->
    if Union_find.find du a = maxcc_root then
      ignore (Sc.unsafe_mut_add_facet facet new_sc) 
  ) sc ();
  new_sc

