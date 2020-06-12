open! Base
open! Import

let lazy_t =
  [%accessor Accessor.isomorphism ~get:(fun info -> lazy info) ~construct:Info.of_lazy_t]
;;
