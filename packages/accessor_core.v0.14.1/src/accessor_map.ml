open! Core_kernel
open! Import
include Accessor.Map

(* This accessor is not well behaved in that it doesn't prevent you from setting the wrong
   keys, so we don't expose it in the interface. *)
let at_multi keys =
  Accessor.field
    ~get:(fun t -> Map.of_key_set keys ~f:(Map.find t))
    ~set:(fun t replacements ->
      Map.fold replacements ~init:t ~f:(fun ~key ~data t ->
        match data with
        | None -> Map.remove t key
        | Some data -> Map.set t ~key ~data))
;;

let at_key_set keys = at_multi keys @> each
let at_key_seti keys = at_multi keys @> eachi
let found_key_set keys = at_key_set keys @> Accessor.Option.some
let found_key_seti keys = at_key_seti keys @> Accessor.Option.some
