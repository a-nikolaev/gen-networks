
let max_facet_size com = 
  Sc.fold_facets (fun a _ acc ->
    let size = Sc.facet_size a com in
    max size acc
  ) com 0

(* accumulate facet degree correlation deterministically *)
let accum_determ com =
  let max_ft_sz = max_facet_size com in

  (* number of facets of each size *)
  let size_hist = Array.make (max_ft_sz+1) 0 in
  (* Hash tables of counts of di, if smapling one node *)
  let arr1 = Array.init (max_ft_sz+1) (fun i -> Hashtbl.create 1) in
  (* Hash tables of counts of dj, if smapling one node *)
  let arr2 = Array.init (max_ft_sz+1) (fun i -> Hashtbl.create 1) in
  (* Hash tables of joint counts (di, dj) *)
  let arr_pair = Array.init (max_ft_sz+1) (fun i -> Hashtbl.create 1) in

  let add ht key =
    let v = try Hashtbl.find ht key with Not_found -> 0 in
    Hashtbl.replace ht key (v+1)
  in
    
  let add1 size (di : int) = add (arr1.(size)) di in
  let add2 size (di : int) = add (arr2.(size)) di in
  let add_pair size (didj : int*int) = add (arr_pair.(size)) didj in

  Sc.fold_facets (fun a facet () ->
    
    let size = Sc.facet_size a com in

    size_hist.(size) <- size_hist.(size) + 1;

    Sc.S.iter (fun i -> 
      let di = Sc.facet_degree i com in
      add1 size di;
      add2 size di;
      Sc.S.iter (fun j ->
        if i <> j then
          let dj = Sc.facet_degree j com in
          add_pair size (di, dj)
      ) facet
    ) facet
  ) com ();
  (size_hist, arr1, arr2, arr_pair)
  

(* accumulate facet degree correlation random *)
let accum_random com =
  let max_ft_sz = max_facet_size com in

  (* Conditional on size joint counts (di, dj), i <> j *)
  let s_pair = Array.init (max_ft_sz+1) (fun i -> Hashtbl.create 1) in
  (* Conditional on size joint counts (di, dj) when i could be equal j *)
  let s_one_one = Array.init (max_ft_sz+1) (fun i -> Hashtbl.create 1) in
  (* All (di, dj) when i <> j *)
  let all_pair = Hashtbl.create 1 in
  (* All (di, dj) when i could be equal j *)
  let all_one_one = Hashtbl.create 1 in

  let all_random_pair = Hashtbl.create 1 in
  let all_random_one_one = Hashtbl.create 1 in

  let add ht key =
    let v = try Hashtbl.find ht key with Not_found -> 0 in
    Hashtbl.replace ht key (v+1)
  in

  let add_pair () =
    match Sc.sample_facet_id com with
    | None -> ()
    | Some a ->
        begin
          let s = Sc.facet_size a com in

          let nodes = Sc.S.elements (Sc.T.ft a com) in

          let sample () = Common.Sample.from_list_opt nodes in
          match sample(), sample() with
          | Some i, Some j ->
              let di = Sc.facet_degree i com in
              let dj = Sc.facet_degree j com in

              if i <> j then begin
                add s_pair.(s) (di, dj);
                add all_pair (di, dj);
              end;
              add s_one_one.(s) (di, dj);
              add all_one_one (di, dj);

          | _ -> ()
        end
  in
  
  let add_random_nodes () =
    match Sc.sample_node com, Sc.sample_node com with
    | Some i, Some j ->
        let di = Sc.facet_degree i com in
        let dj = Sc.facet_degree j com in

        if i <> j then begin
          add all_random_pair (di, dj);
        end;
        add all_random_one_one (di, dj);

    | _ -> ()
  in

  let attempts = 
    let n = Sc.nodes_num com in 
    let m = Sc.facet_num com in 
    m * 100; 
  in

  for attempt = 1 to attempts do
    add_pair();
    add_random_nodes();
  done;

  (s_pair, s_one_one, all_pair, all_one_one, all_random_pair, all_random_one_one)


let () =

  let open Opt in
  let flag, param, _ =
    Opt.scan 
      (* flags  *) [Long "safe"] 
      (* params *) [Long "o"]
      Sys.argv 1
  in
  
  let safe = flag (Long "safe") in

  match param (Long "o") with
  | Some (Opt.OK filename) ->
      let com = Sc.parse_from_chan safe stdin in
      let data = accum_random com in
      let oc = open_out_bin filename in
      Marshal.to_channel oc data [];
      close_out oc
  | _ ->
      Printf.printf "Usage:\n\t ./fd_correlation [--safe] --o=outputfile < scfile \n\n"


