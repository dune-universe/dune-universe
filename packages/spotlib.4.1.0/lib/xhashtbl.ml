open Hashtbl

let replace_list tbl kvs = 
  List.iter (fun (k,v) ->
    Hashtbl.replace tbl k v) kvs

let of_list size kvs =
  let tbl = Hashtbl.create size in
  List.iter (fun (k,v) ->
    Hashtbl.add tbl k v) kvs;
  tbl
  
let to_list tbl = Hashtbl.fold (fun k v st -> (k,v) :: st) tbl []

let find_opt tbl k = try Some (Hashtbl.find tbl k) with Not_found -> None

let find_default def tbl k = try Hashtbl.find tbl k with Not_found -> def

let find_or_add deff tbl k = 
  try Hashtbl.find tbl k with Not_found -> 
    let def = deff k in
    Hashtbl.replace tbl k def;
    def

let alter tbl k f =
  let old = find_opt tbl k in
  match old, f old with
  | None, None -> ()
  | _, Some v -> replace tbl k v
  | Some _, None -> remove tbl k

let concat tbls =
  let t = Hashtbl.create 101 in (* CR jfuruse: fixed *)
  List.iter (Hashtbl.iter (Hashtbl.add t)) tbls;
  t

let create_with size f =
  let tbl = Hashtbl.create size in
  f tbl;
  tbl
