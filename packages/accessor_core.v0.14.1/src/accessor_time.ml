open! Core_kernel
open! Import

let span_since_epoch =
  [%accessor
    Accessor.isomorphism
      ~get:Time.to_span_since_epoch
      ~construct:Time.of_span_since_epoch]
;;

let date_ofday zone =
  Accessor.isomorphism ~get:(Time.to_date_ofday ~zone) ~construct:(fun (date, ofday) ->
    Time.of_date_ofday date ofday ~zone)
;;

let date zone = date_ofday zone @> Accessor.Tuple2.fst
let ofday zone = date_ofday zone @> Accessor.Tuple2.snd
