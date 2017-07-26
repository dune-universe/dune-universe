open Core

let list_hash ~hash xs =
  List.fold xs ~init:0 ~f:(fun h x ->
    Hashtbl.hash (h, hash x))
