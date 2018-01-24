type ('a,'re) t = {
  string : string;
  re : 're;
  binder : left:string -> right:string -> last:string option -> string option array -> 'a;
}


class virtual group named_groups ~left ~right ~last groups_opt = 
  let named_groups_opt = 
    List.map (fun (n,pos) -> n, Array.unsafe_get groups_opt pos) named_groups
  in
  let def = function
    | Some v -> v 
    | None -> ""
  in
  object
    method _groups = (Array.map def groups_opt : string array)
    method _groups_opt = (groups_opt : string option array)
    method _named_groups = (List.map (fun (x,y) -> x, def y) named_groups_opt : (string * string) list)
    method _named_groups_opt = (named_groups_opt : (string * string option) list)
    method _group n = def groups_opt.(n)
    method _group_opt n = groups_opt.(n)
    method _unsafe_group n = def (Array.unsafe_get groups_opt n)
    method _unsafe_group_opt n = Array.unsafe_get groups_opt n
    method _named_group s = def (List.assoc s named_groups_opt)
    method _named_group_opt s = List.assoc s named_groups_opt
    method _left     : string = left
    method _right    : string = right
    method _last     : string = def last
    method _last_opt : string option = last
end

let create string re binder = { string; re; binder }

let make_group_obj rex left groups right =
  let last =  (* probably wrong *)
    let rec find n = 
      if n <= 0 then None
      else
        let gn = Array.unsafe_get groups n in
        match gn with
        | None -> find (n-1)
        | Some _ -> gn
    in
    find (Array.length groups - 1)
  in
  rex.binder ~left ~right ~last groups


