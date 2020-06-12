open! Base
open! Import

let true_ =
  [%accessor
    Accessor.variant
      ~match_:(fun bool -> if bool then First () else Second false)
      ~construct:(fun () -> true)]
;;

let false_ =
  [%accessor
    Accessor.variant
      ~match_:(fun bool -> if bool then Second true else First ())
      ~construct:(fun () -> false)]
;;

let negated = [%accessor Accessor.isomorphism ~get:not ~construct:not]
