open Printf
open Sc
open Common

(*

Usage: 
  
  cat filename | ./walk [-n | -f] -d depth(=3) -N number(=1) [-x] [--safe]

    -n      start with a random node
    -f      start with a random facet
    -N N    number of samples to take (default = 1)
    -d N    walk depth (e.g. N-F-N-F-N is depth 5)
    -x      prohibit walking back the same edge (cannot do N1-F-N1 or F1-N-F1)
    --safe  read the simplicial complex with safety checks (ensuring that no subset facetr are added)

  ./walk < filename

*)

type state = Node of int | Facet of int | DeadEnd


let next_neighbor allow_walk_back com ls =
  let get_random post s =
    match s |> Sc.S.elements |> Sample.from_list_opt with
    | Some x -> post x
    | None -> DeadEnd
  in
  let get_random_facet = get_random (fun x -> Facet x) in
  let get_random_node = get_random (fun x -> Node x) in

  match ls with
  | (Node i) :: tl ->
      let s = Sc.T.nd i com in
      if allow_walk_back then
        s |> get_random_facet
      else
        begin match tl with
        | Facet a :: tl2 -> (s |> Sc.S.remove a) |> get_random_facet
        | _ -> s |> get_random_facet
        end
  | (Facet a) :: tl ->
      let s = Sc.T.ft a com in
      if allow_walk_back then
        s |> get_random_node
      else
        begin match tl with
        | Node i :: tl2 -> (s |> Sc.S.remove i) |> get_random_node
        | _ -> s |> get_random_node
        end
  | _ -> DeadEnd


let () =

  Random.self_init();
  
  let open Opt in
  let flag, param, _ =
    Opt.scan 
      (* flags  *) [Long "safe"; Short 'n'; Short 'f'; Short 'x'] 
      (* params *) [Short 'd'; Short 'N']
      Sys.argv 1
  in
  
  let safe = flag (Long "safe") in

  let com = Sc.parse_from_chan safe stdin in

  let sample_state0 =
    if flag (Short 'n') then 
      (fun () ->
        match Sc.sample_node com with
        | Some i -> Node i
        | _ -> DeadEnd 
      )
    else if flag (Short 'f') then
      (fun () ->
        match Sc.sample_facet_id com with
        | Some a -> Facet a
        | _ -> DeadEnd
      )
    else
      (fun () -> DeadEnd)
  in

  let allow_walk_back = not (flag (Short 'x')) in
  
  let number = match param (Short 'N') with
    | Some (Opt.OK v) -> int_of_string v
    | _ -> 1
  in

  let depth = match param (Short 'd') with
    | Some (Opt.OK v) -> int_of_string v
    | _ -> 3
  in

  let rec walk d ls =
    if d > 0 then
      let x = next_neighbor allow_walk_back com ls in
      walk (d-1) (x :: ls)
    else
      ls
  in

  let print_trace ls = 
    let ss =
      ls |> List.map (function
        | Node i -> Sc.facet_degree i com |> string_of_int
        | Facet a -> Sc.facet_size a com |> string_of_int
        | DeadEnd -> "-"
      )
    in
    ss |> List.iter (fun s -> Printf.printf "%s\t" s);
    Printf.printf "\n"
  in

  for i = 1 to number do
    let state0 = sample_state0 () in
    [state0] |> walk (depth-1) |> List.rev |> print_trace
  done

