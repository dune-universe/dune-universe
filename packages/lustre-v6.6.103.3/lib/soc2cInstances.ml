(* Time-stamp: <modified the 18/03/2016 (at 10:35) by Erwan Jahier> *)


module SocKey = struct
  type t = Soc.key
  let compare = compare
end

module SkMap = Map.Make(SocKey)

(* List.nth^-1 *)
let (get_pos : 'a -> 'a list -> int) =
  fun x l ->
    let rec aux c = function
      | [] -> assert false (* SNO *)
      | y::tail -> if x = y then c else aux (c+1) tail
    in  
    let pos = aux 0 l in
    assert (List.nth l pos = x);
    pos

(** gathers instances of the same soc  into an array *)

open Soc
(* exported *)

let find k t = try SkMap.find k t with Not_found -> 
  Printf.printf "*** SNO: %s not found in %s\n" (Std.dump k) (Std.dump t);
  flush stdout;
  assert false

let to_array :
      ((ident * Soc.key) list -> (Soc.key * int) list * (ident * Soc.key -> int)) =
  fun l -> 
    let rec aux tab = function
      | [] -> tab
      | (id,key)::tail ->
        if SkMap.mem key tab then
          let idl = find key tab in
          let tab = SkMap.add key (id::idl) tab in
          aux tab tail
        else 
          let tab = SkMap.add key [id] tab in
          aux tab tail
    in
    let tab = aux SkMap.empty l in
    let il = SkMap.fold (fun sk idl acc -> (sk, List.length idl)::acc) tab [] in
    let inst_to_index (id,sk) = 
      let idl = find sk tab in
      let i = get_pos id (List.rev idl) in
      i
    in 
    il, inst_to_index 

    
