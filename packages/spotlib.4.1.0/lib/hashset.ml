(* poorman's hash set by hashtbl *)
type 'a t = ('a, 'a) Hashtbl.t

let create = Hashtbl.create
let add set x = Hashtbl.replace set x x
let remove = Hashtbl.remove
let mem = Hashtbl.mem
let find = Hashtbl.find
let find_opt t k = try Some (Hashtbl.find t k) with Not_found -> None

let find_or_add t k = 
  try Hashtbl.find t k with Not_found -> 
    add t k; k
    
let iter f = Hashtbl.iter (fun v _ -> f v)
let fold f = Hashtbl.fold (fun v _ st -> f v st)
let elements = Hashtbl.length
let clear = Hashtbl.clear

let of_list size vs = 
  let set = create size in
  List.iter (add set) vs;
  set

let to_list set = fold (fun x y -> x::y) set []

module Make(A : Hashtbl.HashedType) = struct
  
  module Hashtbl = Hashtbl.Make(A)

  type t = A.t Hashtbl.t
  
  let create = Hashtbl.create
  let add set x = Hashtbl.replace set x x
  let remove = Hashtbl.remove
  let mem = Hashtbl.mem
  let find = Hashtbl.find
  let find_opt t k = try Some (Hashtbl.find t k) with Not_found -> None
  
  let find_or_add t k = 
    try Hashtbl.find t k with Not_found -> 
      add t k; k
      
  let iter f = Hashtbl.iter (fun v _ -> f v)
  let fold f = Hashtbl.fold (fun v _ st -> f v st)
  let elements = Hashtbl.length
  let clear = Hashtbl.clear
  
  let of_list size vs = 
    let set = create size in
    List.iter (add set) vs;
    set
  
  let to_list set = fold (fun x y -> x::y) set []
  
end
