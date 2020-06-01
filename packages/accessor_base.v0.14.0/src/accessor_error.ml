open! Base
open! Import

let info = [%accessor Accessor.isomorphism ~get:Error.to_info ~construct:Error.of_info]

let lazy_t =
  [%accessor
    Accessor.isomorphism ~get:(fun error -> lazy error) ~construct:Error.of_lazy_t]
;;
