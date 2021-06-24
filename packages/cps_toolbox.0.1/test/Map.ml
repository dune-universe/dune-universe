open Cps_toolbox
open Functional

let map_gen key_order key_gen value_gen st =
  let open QCheck.Gen in
  List.sort_unique key_order (list key_gen st) |> fun keys ->
  List.map (fun key -> key, value_gen st) keys |> fun entries ->
  Map.from_entries entries |> identity

let map_shrink binds yield =
  QCheck.Shrink.list (Map.entries binds) (yield <== Map.from_entries)

let map_print key_print value_print map =
  let open Printf in
  let _bind_print (key, value) =
    sprintf "%s : %s" (key_print key) (value_print value)
  in
  let rec _print binds return =
    match binds with
    | [] -> return ""
    | bind :: [] -> return (_bind_print bind)
    | bind :: binds' ->
      _print binds' (return <== (sprintf "%s, %s" (_bind_print bind)))
  in
  _print (Map.entries map) (sprintf "{%s}")

let arbitrary_map key_order k v =
  QCheck.make (map_gen key_order k.gen v.gen)
    ?print:(Option.map2 map_print QCheck.(k.print) QCheck.(v.print))
    ~shrink:map_shrink

(* Define tests *)
let insert_is_contained =
  QCheck.Test.make ~count:1000
    ~name:"insert_is_contained"
    QCheck.(triple int int (arbitrary_map Order.int int int))
    (fun (key, value, binds) ->
      Map.insert Order.int key value binds |> fun binds1 ->
      Map.contains Order.int key binds1
        (fun () -> false)
        (fun () -> true))

let remove_not_contained =
  QCheck.Test.make ~count:1000
    ~name:"remove_not_contained"
    QCheck.(pair int (arbitrary_map Order.int int int))
    (fun (key, binds) ->
      Map.remove Order.int key binds |> fun binds1 ->
      Map.contains Order.int key binds1
        (fun () -> true)
        (fun () -> false))

let contained_lookup_is_same =
  QCheck.Test.make ~count:1000
    ~name:"contained_lookup_is_same"
    QCheck.(triple int int (arbitrary_map Order.int int int))
    (fun (key, value, binds) ->
      Map.insert Order.int key value binds |> fun binds1 ->
      Map.lookup Order.int key binds1
        (fun () -> false)
        (fun value1 -> value = value1))

let not_contained_lookup_is_none =
  QCheck.Test.make ~count:1000
    ~name:"not_contained_lookup_is_none"
    QCheck.(pair int (arbitrary_map Order.int int int))
    (fun (key, binds) ->
      Map.remove Order.int key binds |> fun binds1 ->
      Map.lookup Order.int key binds1
        (fun () -> true)
        (fun _value -> false))

(* Expose tests *)
let test_suite =
  [ insert_is_contained
  ; remove_not_contained
  ; contained_lookup_is_same
  ; not_contained_lookup_is_none
  ]
