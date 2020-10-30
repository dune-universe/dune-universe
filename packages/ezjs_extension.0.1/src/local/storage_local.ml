open Ezjs_min

let local_storage f =
  match Optdef.to_option Dom_html.window##.localStorage with
  | None -> ()
  | Some storage -> f storage

let get_item_storage storage key =
  match Opt.to_option @@ storage##getItem (string key) with
  | None -> Printf.printf "no storage value for %S" key; None
  | Some v ->
    if v = string "undefined" || v = string "null" then None
    else Some (_JSON##parse v)

let get_item key f = local_storage @@ fun storage ->
  match get_item_storage storage key with
  | None -> ()
  | Some v -> f v

let get_items keys f = local_storage @@ fun storage ->
  let l = List.map (fun k -> k, Unsafe.inject @@ Optdef.option @@ get_item_storage storage k) keys in
  f @@ Unsafe.obj (Array.of_list l)

let get_keys f = local_storage @@ fun storage ->
  let n = storage##.length in
  let l = List.init n (fun i -> storage##key i) in
  f @@ List.rev @@ List.fold_left (fun acc x -> match Opt.to_option x with
      | None -> acc
      | Some s -> to_string s :: acc) [] l

let set_item key value = local_storage @@ fun storage ->
  storage##setItem (string key) (_JSON##stringify value)

let get ?key _ f = match key with
  | Some k -> get_item k f
  | None -> get_keys (fun keys -> get_items keys f)

let get_arr ?keys _ f = match keys with
  | Some ks -> get_items ks f
  | None -> get_keys (fun keys -> get_items keys f)

let get_o ?obj _ f = match obj with
  | Some o -> let keys = List.map to_string @@ Array.to_list (to_array (object_keys o)) in
    get_items keys f
  | None -> get_keys (fun keys -> get_items keys f)

let set ?callback _ o =
  let keys = Array.to_list (to_array (object_keys o)) in
  List.iter (fun k ->
      let v = Unsafe.get o k in
      set_item (to_string k) v) keys;
  match callback with None -> () | Some f -> f ()

let remove ?callback _ s = local_storage @@ fun storage ->
  storage##removeItem (string s);
  match callback with None -> () | Some f -> f ()

let clear ?callback _ = local_storage @@ fun storage ->
  storage##clear;
  match callback with None -> () | Some f -> f ()

let sync = ()
let local = ()
