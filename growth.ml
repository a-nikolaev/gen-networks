open Printf
open Sc
open Evolve

open Undergrowth

let error s = 
  prerr_string ("Error: "^ s ^ "\n");
  prerr_string ("Usage:\n\
    \t./growth OPTIONS \n\
    \t./growth OPTIONS initial-sc-filename \n\n\
    \t EXAMPLE:\n\
    \t./growth --type=sc --model=acl:2:0.5,1,1 --any --n=100 \n\n\
    \t OPTIONS:\n\n\
    \t--type=[sc|hg]  Simplicial complex or hypergraph\n\
    \t--model=[acl:var:num,num,num | sizedist:filename | aclsizedist:var:num,num,num:filename] ACL:1 ACL:2 ACL:3 model or Basu's facet size distribution model \n\n\
    \t--any           OR\n\
    \t--best=metric   Random growth or maximizing a metric\n\n\
    \t--n=num         OR\n\
    \t--lcc=num       OR
    \t--i=num         Grow until have --n nodes, --lcc max CC size, or until performed --i operations \n\n\
    \t--seed=num      Optional random seed\n\
    \t--virt=filename Optional virtual graph output\n\
    \t--safe          Optional safe reading the initial network file \n");
  exit 1

(* read facet size distribution from a file *)
let read_fs_dist_from_file filename =
  let ic = open_in filename in
  let fpn = ic |> input_line |> float_of_string in 

  let rx = Str.regexp "[ \t]+" in

  let rec next () =
    try 
      let line = input_line ic |> String.trim in
      begin match Str.split rx line with
      | a :: b :: _ -> 
          let v = a |> int_of_string in
          let p = b |> float_of_string in
          (v, p) :: next ()
      | [] -> []
      | _ -> failwith "Growth: sizedist is malformed"
      end
    with
      | End_of_file -> []
  in
  let dist_ls = next () in
  close_in ic;
  (fpn, dist_ls)

(* read distribution from a file *)
let read_any_dist_from_file filename =
  let ic = open_in filename in
  
  let rx = Str.regexp "[ \t]+" in

  let rec next () =
    try 
      let line = input_line ic |> String.trim in
      begin match Str.split rx line with
      | a :: b :: _ -> 
          let v = a |> int_of_string in
          let p = b |> float_of_string in
          (v, p) :: next ()
      | [] -> []
      | _ -> failwith "Growth: sizedist is malformed"
      end
    with
      | End_of_file -> []
  in
  let dist_ls = next () in
  close_in ic;
  dist_ls

let _ =
  let open Opt in
  let flag, param, other_args =
    Opt.scan 
      (* flags  *) [Long "safe"; Long "any"] 
      (* params *) [Long "seed"; Long "type"; Long "model"; Long "best"; Long "i"; Long "n"; Long "lcc"; Long "virt"]
      Sys.argv 1
  in
 
  let model =
    match param (Long "model") with
    | Some (OK s) -> 
        begin match String.split_on_char ':' s with
        | "acl" :: var :: tl -> 
            let params = String.concat "" tl in
            let nums = String.split_on_char ',' params in
            begin match nums with
            | a::c::l::[] ->
                Evolve.ACL (int_of_string var, (float_of_string a, int_of_string c, int_of_string l))
            | _ ->
                error "Option --model=acl:var:a,c,l Provide four parameters -- a,c,L -- separated by commas"
            end
        | "acdistl" :: var :: tl -> 
            let params = String.concat "" tl in
            let nums = String.split_on_char ',' params in
            begin match nums with
            | a::cdist::l::[] ->
                Evolve.ACdistL (int_of_string var, (float_of_string a, read_any_dist_from_file cdist, int_of_string l))
            | _ ->
                error "Option --model=acdistl:var:a,cdistfile,l"
            end
        | "sizedist" :: tl ->
            let filename = String.concat "" tl in
            let fpn, dist_ls = read_fs_dist_from_file filename in
            Evolve.SizeDist (fpn, dist_ls)
        | "aclsizedist" :: var :: acl :: tl ->
            let nums = String.split_on_char ',' acl in
            begin match nums with
            | a::c::l::[] ->
                (* read the facet size distribution *)
                let filename = String.concat "" tl in
                let dist_ls = read_any_dist_from_file filename in
                Evolve.ACLSizeDist (int_of_string var, (float_of_string a, int_of_string c, int_of_string l), dist_ls)
            | _ ->
                error "Option --model=aclsizedist:var:a,c,l:filename"
            end
        | _ ->
            error "Either --model=acl:var:a,c,l or --model=sizedist:filename is expected"
        end
    | _ ->
        error "Either --model=acl:a,c,l or --model=sizedist:filename is expected"
  in
    
  let control_method =
    match param (Long "best"), flag (Long "any") with
    | Some (OK _), true ->
        error "Either --any or --best=metric option is required, but not both"
    | Some (OK m), false ->
        (Best (int_of_string m))
    | _, true ->
        Any
    | _ ->
        error "Either --any or --best=metric option is required"
  in
  
  let stopping =
    match param (Long "i"), param (Long "n"), param (Long "lcc") with
    | Some (OK s), None, None -> 
        let count = int_of_string s in
        (fun i _ -> i >= count)
    | None, Some (OK s), None ->
        let count = int_of_string s in
        (fun _ com -> nodes_num com >= count)
    | None, None, Some (OK s) ->
        let count = int_of_string s in
        let next_m = ref count in
        (fun i com -> 
           (* Printf.fprintf stderr "\t\t ----> %i \n" (!next_m); *)
           let m = Sc.facet_num com in
           if m >= !next_m then begin
             let cc_size = Concomp.largest_cc_size com in
             let m_estimate = count * m / cc_size in
             next_m := (!next_m + m_estimate)/2 + 1;
             cc_size >= count
           end else
             false
        )
    | _ ->
        error "Either --i=number or --n=number or --lcc=number option is required"
  in

  begin match param (Long "seed") with
  | Some (OK s) ->
      let seed = int_of_string s in
      Random.init seed
  | _ ->
      Random.self_init()
  end;
  
  let initial_file = match other_args with
    | hd :: _ -> Some hd
    | _ -> None
  in

  let com0 = 
    match initial_file with
    | Some file -> 
        let add_facets_safe = flag (Long "safe") in
        Sc.parse_from_file add_facets_safe file
    | None -> 
        let obj_type =
          match param (Long "type") with
          | Some (OK ot) ->
              begin match ot with 
              | "sc" -> Sc.SimplicialComplex
              | "hg" -> Sc.Hypergraph
              | _ -> error "Option --type is not specified, must be 'sc' or 'hg'"
              end
          | _ -> 
              error "Option --type is not specified, must be 'sc' or 'hg'"
        in
        make_unit_com obj_type
  in
  
  let opt_virt_graph_info = 
    match param (Long "virt") with
    | Some (OK virt_output_filename) ->
        (* make an empty virtual graph *)
        let gr = Virt_graph.make_empty() in
        (* add all present facets to the virtual graph structure as singleton virt-nodes *)
        Sc.fold_facets (fun a _ () -> 
            Virt_graph.add_singleton_node gr a
          ) com0 ();
        Some (gr, virt_output_filename)
    | _ -> 
        None
  in

  let _, com = 
    grow control_method model
      stopping
      (fun _ com -> 
        (* Printf.fprintf stderr "%i \t %i\n%!" (Sc.nodes_num com) (Sc.facet_num com); *)
        ()
      )
      opt_virt_graph_info
      com0
  in

  (* save virtual graph *)
  begin match opt_virt_graph_info with
  | Some (gr, filename) -> Virt_graph.save_bin gr filename
  | None -> ()
  end;

  
  (* print SC *)
  Sc.fprint_normal stdout com;
  printf "%!"
  

  (*
  (* print the size of the largest CC (in terms of the number of facets/hyperedges) *)
  let maxcc = Concomp.largest_cc_size com in
  fprintf stdout "%i\n" maxcc
  *)
