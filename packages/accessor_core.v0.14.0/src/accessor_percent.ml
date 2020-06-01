open! Core_kernel
open! Import

let mult =
  [%accessor Accessor.isomorphism ~get:Percent.to_mult ~construct:Percent.of_mult]
;;

let percentage =
  [%accessor
    Accessor.isomorphism ~get:Percent.to_percentage ~construct:Percent.of_percentage]
;;

let bp = [%accessor Accessor.isomorphism ~get:Percent.to_bp ~construct:Percent.of_bp]

let scaled s =
  [%accessor
    Accessor.isomorphism
      ~get:(fun p -> Percent.scale p s)
      ~construct:(fun p -> Percent.scale p (1. /. s))]
;;
