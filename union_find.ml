
type entry = { 
  v : int;
  mutable parent : entry option;
  mutable size : int ;
}

type disjoint_union = 
  { entries : (int, entry) Hashtbl.t;
    roots : (int, unit) Hashtbl.t;
    mutable num : int; (* number of disjoint sets *)
  }

let get_entry du x =
  try
    Hashtbl.find du.entries x
  with
  | Not_found -> failwith "Union_find: get_entry: entry not found"

let parent e =
  match e.parent with
  | None -> failwith "Union_find: parent: parent is not defined"
  | Some ep -> ep

let find_root_entry du x =
  let rec loop e ep =
    if ep.v <> e.v then begin
      let epp = parent ep in
      e.parent <- Some epp;
      loop ep epp
    end else
      ep
  in
  let e = get_entry du x in
  loop e (parent e)

(* Programming Interface *)
let empty () =
  { entries = Hashtbl.create 10; roots = Hashtbl.create 10; num = 0 }

let mem du x = Hashtbl.mem du.entries x 

let add du x =
  if mem du x then
    failwith "Union_find: entry already exists"
  else begin
    let e = { v = x; parent = None; size = 1 } in
    e.parent <- Some e;
    Hashtbl.replace du.entries x e;
    Hashtbl.replace du.roots x ();
    du.num <- du.num + 1
  end

let union du x y =
  let ex = find_root_entry du x in
  let ey = find_root_entry du y in

  if ex.v <> ey.v then begin
    du.num <- du.num - 1;
    if ex.size > ey.size then begin
      ex.size <- ex.size + ey.size;
      ey.parent <- Some ex;
      Hashtbl.remove du.roots ey.v
    end else begin
      ey.size <- ey.size + ex.size;
      ex.parent <- Some ey;
      Hashtbl.remove du.roots ex.v
    end
  end

let find du x = 
  (find_root_entry du x).v

(* Extra interface *)
(* Size of set that contains x *)
let size du x =
  (find_root_entry du x).size

(* List of all root elements *)
let all_roots du =
  du.roots |> Hashtbl.to_seq_keys |> List.of_seq

(* List of set sizes *)
let all_sizes du =
  du |> all_roots |> List.rev_map (size du) |> List.rev

