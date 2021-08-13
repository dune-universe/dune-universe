open Functional

type ('key, 'value) data =
  | Peek of 'key
  | Bind of 'key * 'value

type ('key, 'value) map = (('key, 'value) data) AVL.tree

let _peek key = Peek key
let _bind key value = Bind (key, value)

let get_key data =
  match data with
  | Peek key -> key
  | Bind (key, _) -> key

let get_value data fail return =
  match data with
  | Peek _ -> fail ()
  | Bind (_, value) -> return value

let get_value_unsafe data return =
  match data with
  | Peek _ -> assert false
  | Bind (_, value) -> return value

let empty = AVL.null
let size = AVL.get_count

let fold empty_case bind_case binds =
  AVL.to_list binds |> fun binds1 ->
  List.fold
    (fun return -> return empty_case)
    (fun bind visit_binds return ->
      match bind with
      | Peek _ -> assert false
      | Bind (key, value) ->
        visit_binds @@ fun result ->
        return (bind_case key value result))
    binds1 identity

let map f binds =
  AVL.map
    (fun bind ->
      match bind with
      | Peek _ -> assert false
      | Bind (key, value) ->
        _bind key (f value))
    binds

let contains key_order key binds fail return =
  let _bind_order a b = key_order (get_key a) (get_key b) in
  AVL.is_member _bind_order (_peek key) binds fail return

let insert key_order key value binds =
  let _data_order left right = key_order (get_key left) (get_key right) in
  AVL.insert _data_order (_bind key value) binds

let remove key_order key binds =
  let _data_order left right = key_order (get_key left) (get_key right) in
  AVL.remove _data_order (_peek key) binds

let lookup order key binds fail return =
  let open AVL in
  let open Order in
  let rec _visit tree =
    match tree with
    | Null -> fail ()
    | Node (_, _, data, left, right) ->
      begin match order key (get_key data) with
      | EQ -> get_value data fail return
      | LT -> _visit left
      | GT -> _visit right
      end
  in
  _visit binds

let lookup_unsafe order key binds =
  let open AVL in
  let open Order in
  let rec _visit tree =
    match tree with
    | Null -> assert false
    | Node (_, _, data, left, right) ->
      match order key (get_key data) with
      | EQ -> get_value_unsafe data identity
      | LT -> _visit left
      | GT -> _visit right
  in
  _visit binds

let entries binds =
  fold
    (fun return -> return [])
    (fun key value visit_binds return ->
      visit_binds @@ fun result ->
      return ((key, value) :: result))
    binds identity

let keys binds =
  fold
    (fun return -> return [])
    (fun key _value visit_binds return ->
      visit_binds @@ fun result ->
      return (key :: result))
    binds identity

let values binds =
  fold
    (fun return -> return [])
    (fun _key value visit_binds return ->
      visit_binds @@ fun result ->
      return (value :: result))
    binds identity

let from_entries entries =
  List.map (fun (key, value) -> _bind key value) entries |> fun binds ->
  AVL.from_list binds
