let mem f = 
  let ht = Hashtbl.create 100 in
  ( fun x -> 
      try Hashtbl.find ht x with
        Not_found ->
        ( let new_res = f x in
          Hashtbl.add ht x new_res;
          new_res        
        )
  )

let mem_rec f = 
  let ht = Hashtbl.create 100 in
  let rec self =
  ( fun x -> 
      try Hashtbl.find ht x with
        Not_found ->
        ( let new_res = f self x in
          Hashtbl.add ht x new_res;
          new_res        
        )
  )
  in
  self
