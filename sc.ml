open Printf

open Common

module S = Set.Make(struct type t = int let compare = compare end)

type s = S.t

module Sampler = struct
  type t = {
    mutable size: int; 
    mutable value: int array;
    id: (int, int) Hashtbl.t;
  }

  let capacity sampler = Array.length sampler.value

  let make () = {size = 0; value = Array.make 4 0; id = Hashtbl.create 4} 

  let copy sampler = 
    { size = sampler.size; 
      value = Array.copy sampler.value;
      id = Hashtbl.copy sampler.id;
    }

  let mem v sampler = Hashtbl.mem sampler.id v

  let mut_add v sampler = 
    if not (mem v sampler) then
    ( if capacity sampler <= sampler.size then
      (
        (* increase the array size *)
        let arr = Array.init (sampler.size * 2) (fun i -> if i < sampler.size then sampler.value.(i) else 0) in
        sampler.value <- arr
      );

      let i = sampler.size in 
      sampler.value.(i) <- v;
      sampler.size <- i+1;
      Hashtbl.add sampler.id v i
      
    )

  let mut_remove v sampler =
    try 
      let i = Hashtbl.find sampler.id v in
      let j = sampler.size-1 in
      
      if j > i then
      ( let vj = sampler.value.(j) in
        sampler.value.(i) <- vj;
        Hashtbl.replace sampler.id vj i
      );

      Hashtbl.remove sampler.id v;
      sampler.size <- sampler.size - 1
    with
      Not_found -> ()

  let sample_uniform sampler = 
    if sampler.size > 0 then
      Some (let i = Random.int sampler.size in sampler.value.(i))
    else
      None

  let fprint ch sampler =
    Array.iteri (fun i v -> 
      fprintf ch "[%i]=%i " i v;
      if i = sampler.size - 1 then
        fprintf ch " | "
    ) sampler.value;
    fprintf ch "\n";
    Hashtbl.iter(fun v id -> 
      fprintf ch "%i->%i " v id;
    ) sampler.id;
    fprintf ch "\n"

end

type object_type = SimplicialComplex | Hypergraph

module T = struct
  type t = {
    mutable nodes : s;

    mutable ft_id_bound: int;

    obj_type: object_type;

    ft: (int,s) Hashtbl.t;
    nd: (int,s) Hashtbl.t; 

    ft_size: (int, int) Hashtbl.t;
    nd_size: (int, int) Hashtbl.t;

    ft_sampler : Sampler.t;
    nd_sampler : Sampler.t;
  }

  let ft a com = Hashtbl.find com.ft a
  let nd i com = Hashtbl.find com.nd i
end


open T
  
let fprint_set ch s =
  fprintf ch "{ ";
  S.iter (fun n -> fprintf ch "%i " n) s;
  fprintf ch "}"

let fprint_verbose ch com =
  fprintf ch "Nodes: ";
  fprint_set ch com.nodes;
  fprintf ch "\n";

  printf "Facets: \n";
  Hashtbl.iter(fun a s -> 
    fprintf ch "\t%i -> " a;
    fprint_set ch s;
    fprintf ch "\n";
  ) com.ft;

  printf "Nodes belong to: \n";
  Hashtbl.iter(fun i s -> 
    fprintf ch "\t%i is in " i;
    fprint_set ch s;
    fprintf ch "\n";
  ) com.nd;

  Sampler.fprint ch com.ft_sampler;
  Sampler.fprint ch com.nd_sampler

let fprint_normal ch com =
  begin match com.obj_type with
  | SimplicialComplex -> fprintf ch "%%sc\n"
  | Hypergraph -> fprintf ch "%%hg\n"
  end;
  Hashtbl.iter(fun a s -> 
    fprint_set ch s;
    fprintf ch "\n";
  ) com.ft


let make_empty obj_type =
  { nodes = S.empty; obj_type; ft_id_bound = -1; 
    ft = Hashtbl.create 4; nd = Hashtbl.create 4; 
    ft_size = Hashtbl.create 4; nd_size = Hashtbl.create 4; 
    ft_sampler = Sampler.make(); nd_sampler = Sampler.make() }

let copy com = 
  { nodes = com.nodes;
    obj_type = com.obj_type;
    ft_id_bound = com.ft_id_bound;
    ft = Hashtbl.copy com.ft;
    nd = Hashtbl.copy com.nd;
    ft_size = Hashtbl.copy com.ft_size;
    nd_size = Hashtbl.copy com.nd_size;
    ft_sampler = Sampler.copy com.ft_sampler;
    nd_sampler = Sampler.copy com.nd_sampler;
  }

let facet_num com =
  Hashtbl.length com.ft

let nodes_num com =
  Hashtbl.length com.nd

let fold_facets f com acc =
  Hashtbl.fold (fun a facet acc -> f a facet acc) com.ft acc

let fold_nodes_in_facet f (com, a) acc =
  try 
    let facet = Hashtbl.find com.ft a in
    S.fold (fun i acc -> f i acc) facet acc
  with
    Not_found -> failwith "fold_nodes_in_facet: Not_found"

let fold_nodes f com acc =
  Hashtbl.fold (fun i nbrs acc -> f i nbrs acc) com.nd acc


let facet_size a com = 
  try
    (*
    let x = Hashtbl.find com.ft a |> S.cardinal in
    assert (x = Hashtbl.find com.ft_size a);
    x
    *)
    Hashtbl.find com.ft_size a
  with 
    Not_found -> failwith "facet_size: Not_found"

let facet_degree i com = 
  try
    (*
    let x = Hashtbl.find com.nd i |> S.cardinal in
    assert (x = Hashtbl.find com.nd_size i);
    x
    *)
    Hashtbl.find com.nd_size i
  with 
    Not_found -> failwith "facet_degree: Not_found"


(* find the list of ids of all facets that are subsets of s*)
let find_subset_facet_id s com =

  let ftnum1 = S.fold (fun n acc -> (try Hashtbl.find com.nd_size n with Not_found -> 0) + acc) s 0 in
  let ftnum2 = facet_num com in

  if ftnum1 < ftnum2 then
  ( let marked_ft = Hashtbl.create ftnum1 in
    S.iter (fun n -> 
      
      if Hashtbl.mem com.nd n then
      ( let neighbors = nd n com in
        S.iter (fun a -> 
          if not (Hashtbl.mem marked_ft a) then
            Hashtbl.replace marked_ft a (S.subset (ft a com) s)
        ) neighbors
      ) 
    ) s;
    Hashtbl.fold (fun a is_subset acc -> if is_subset then a::acc else acc) marked_ft []
  )
  else
    Hashtbl.fold (fun a face acc -> if S.subset face s then a::acc else acc) com.ft []

(* find at least one superset of the facet s *)
exception Found_superset of int
let find_a_superset_facet_id s com =
  let ftnum1 = S.fold (fun n acc -> (try Hashtbl.find com.nd_size n with Not_found -> 0) + acc) s 0 in
  let ftnum2 = facet_num com in
  
  try
    if S.is_empty s then raise (Found_superset 0);

    if ftnum1 < ftnum2 then
    ( let marked_ft = Hashtbl.create ftnum1 in
      S.iter (fun n -> 
        if Hashtbl.mem com.nd n then
        ( let neighbors = nd n com in
          S.iter (fun a -> 
            if not (Hashtbl.mem marked_ft a) then
            ( let is_a_superset = S.subset s (ft a com) in
              if is_a_superset then raise (Found_superset a)
              else
                Hashtbl.replace marked_ft a false
            )
          ) neighbors
        ) 
      ) s;
      None
    )
    else
    ( Hashtbl.iter (fun a face -> if S.subset s face then raise (Found_superset a)) com.ft;
      None
    )
  with Found_superset a -> Some a

let mut_remove_facet_id a com =
  let facet = (ft a com) in
  (* remove it from the nodes it contains *)
  S.iter (fun i -> 
    let nbhd = S.remove a (nd i com) in
    if S.is_empty nbhd then
    ( com.nodes <- S.remove i com.nodes;
      Hashtbl.remove com.nd i;
      Hashtbl.remove com.nd_size i;
      Sampler.mut_remove i com.nd_sampler
    )
    else
    ( Hashtbl.replace com.nd i nbhd;
      Hashtbl.replace com.nd_size i (Hashtbl.find com.nd_size i - 1)
    )
  ) facet;
  (* remove the facet *)
  Hashtbl.remove com.ft a;
  Hashtbl.remove com.ft_size a;
  Sampler.mut_remove a com.ft_sampler

type addition_outcome = Added of int | NotAdded of S.t | AddedWithDeletion of (int * (int list))

let unsafe_mut_add_facet facet com =
  (* pick a new facet id *)
  let a = com.ft_id_bound + 1 in
  com.ft_id_bound <- a;
  
  (* add new facet *)
  Hashtbl.replace com.ft a facet;
  Hashtbl.replace com.ft_size a (S.cardinal facet);
  Sampler.mut_add a com.ft_sampler;
  
  (* find which nodes are new *)
  let all_new_nodes = S.fold (fun i acc -> if Hashtbl.mem com.nd i then acc else i::acc) facet [] in
  com.nodes <- List.fold_left (fun acc node -> S.add node acc) com.nodes all_new_nodes;
  List.iter(fun i -> Sampler.mut_add i com.nd_sampler) all_new_nodes;

  (* update the nodes *)
  let h i = try Hashtbl.find com.nd i with Not_found -> S.empty in
  let h_size i = try Hashtbl.find com.nd_size i with Not_found -> 0 in
  S.iter(fun i -> 
    Hashtbl.replace com.nd i (S.add a (h i));
    Hashtbl.replace com.nd_size i (1 + (h_size i));
  ) facet;
  
  (* return the new facet id *)
  a
  

let mut_add_facet facet com =

  if com.obj_type = SimplicialComplex then
    match (find_a_superset_facet_id facet com) with
    | Some _ -> NotAdded facet (* it's already there *)
    | None ->
      ( (* remove subsets *)
        let subset_ids = find_subset_facet_id facet com in

        List.iter (fun b ->
          mut_remove_facet_id b com
        ) subset_ids;
    
        let a = unsafe_mut_add_facet facet com in
        
        let response = 
          if subset_ids = [] then
            Added a
          else
            AddedWithDeletion (a, subset_ids)
        in
      
        response
      )
  else
    ( let a = unsafe_mut_add_facet facet com in
      (Added a) )

let mut_add_new_node_to_facet_id a com =
  let i = S.max_elt com.nodes + 1 in
  let ft = Hashtbl.find com.ft a in
  let ft2 = S.add i ft in
  Hashtbl.replace com.ft a ft2;
  Hashtbl.replace com.ft_size a (Hashtbl.find com.ft_size a + 1);
  Hashtbl.replace com.nd i (S.of_list [a]);
  Hashtbl.replace com.nd_size i 1;
  Sampler.mut_add i com.nd_sampler;
  com.nodes <- S.add i com.nodes

let add_facet ft com =
  let com2 = copy com in
  ignore( com2 |> (mut_add_facet ft) );
  com2


let sample_facet_id com =
  Sampler.sample_uniform com.ft_sampler

let sample_node com =
  Sampler.sample_uniform com.nd_sampler

let sample_node_pref com =
  let sum = Hashtbl.fold (fun _ size acc -> acc + size) com.nd_size 0 in
  if sum <= 0 then failwith "Sc: sample_node_pref: network is empty";
  let rec next x seq = 
    match seq () with
    | Seq.Nil -> failwith "Sc: sample_node_pref: sampling error"
    | Seq.Cons ((i, size), rest) -> 
        if x < size then
          i
        else
          next (x - size) rest 
  in
  next (Random.int sum) (Hashtbl.to_seq com.nd_size)

(* skeleton *)

module Edges = Set.Make(struct type t = int*int let compare = compare end)

let skeleton com =
  let e = 
    fold_facets (fun a _ acc_edges ->
      fold_nodes_in_facet (fun i acc -> 
          fold_nodes_in_facet (fun j acc -> 
              if (i < j) then Edges.add (i,j) acc else acc
            )
            (com, a) acc
        ) 
        (com, a) acc_edges 
    ) com Edges.empty
  in

  let ncom = make_empty com.obj_type in

  Edges.iter (fun (i,j) -> 
      ignore (mut_add_facet (S.of_list [i;j]) ncom);
    ) e; 

  ncom


(* parsers *)
let parse_from_chan add_facets_safe ic =

  let nat_of_char c = Char.code c - Char.code '0' in

  let rec scan_set ls optnum =
    match optnum, input_char ic with
    | None, (('0'..'9') as c) -> scan_set ls (Some (nat_of_char c))  
    | Some n, (('0'..'9') as c) -> scan_set ls (Some (n*10 + nat_of_char c))  
    | None, '}' -> ls
    | Some n, '}' -> n::ls
    | Some n, _ -> scan_set (n::ls) None
    | None, _ -> scan_set ls None
  in

  let rec scan ((sets_acc, options_acc) as acc) =
    let opt_c = 
      try 
        Some (input_char ic)
      with
        End_of_file -> None
    in
    match opt_c with
    | Some c ->
      begin match c with
        | '{' -> 
            let set = scan_set [] None in
            scan (set::sets_acc, options_acc)
        | '%' ->
            begin try 
              let line = (input_line ic) |> String.trim |> String.lowercase_ascii in
              scan (sets_acc, line::options_acc)
            with
              End_of_file -> acc
            end
        | _ -> scan acc
      end
    | None -> acc
  in
  
  let ls_sets, ls_options = scan ([], []) in
  let obj_type =
    if List.mem "sc" ls_options then
      SimplicialComplex
    else if List.mem "hg" ls_options then
      Hypergraph
    else begin
      Printf.eprintf "Warning: Sc.parse_from_chan: No object type, %%sc or %%hg specified, %%hg (hypergraph) assumed.\n";
      Hypergraph
    end
  in
  let com = make_empty obj_type in
  List.iter (fun sls -> 
      if add_facets_safe then
        ignore (mut_add_facet (S.of_list sls) com)
      else
        ignore (unsafe_mut_add_facet (S.of_list sls) com)
    ) ls_sets;
  com

let parse_from_file add_facets_safe file = 
  let ic = open_in file in
  let com = parse_from_chan add_facets_safe ic in
  close_in ic;
  com


