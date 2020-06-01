open! Core_kernel
open! Import
include Accessor.List

let zipped_with_remainder =
  [%accessor
    Accessor.isomorphism
      ~get:(fun (xs, ys) -> List.zip_with_remainder xs ys)
      ~construct:(fun (xyz, remainder) ->
        let xs, ys = List.unzip xyz in
        match remainder with
        | None -> xs, ys
        | Some (First xtl) -> xs @ xtl, ys
        | Some (Second ytl) -> xs, ys @ ytl)]
;;

let zipped = [%accessor zipped_with_remainder @> Accessor.Tuple2.fst]
