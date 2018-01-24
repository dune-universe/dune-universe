type hash = int
type 'a t = (hash, Obj.t) Hashtbl.t
type fields = (hash * Obj.t) array

module Internal = struct
    
  let get t k = Obj.magic (Hashtbl.find t k) (* never fails thanks to the type safety *)
  
  let set t k v = Obj.magic (Hashtbl.find t k) := v
  
  let copy_with fields topt =
    let t = match topt with
      | None -> Hashtbl.create (Array.length fields)
      | Some t -> Hashtbl.copy t
    in
    Array.iter (fun (k,o) -> Hashtbl.replace t k o) fields;
    t
  
  let create fields = copy_with fields None

end
