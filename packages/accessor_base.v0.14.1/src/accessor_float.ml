open! Base
open! Import

let negated = [%accessor Accessor.isomorphism ~get:Float.neg ~construct:Float.neg]
let added s = Accessor.isomorphism ~get:(fun a -> a +. s) ~construct:(fun a -> a -. s)
let subtracted s = Accessor.invert (added s)

let multiplied s =
  Accessor.isomorphism ~get:(fun a -> a *. s) ~construct:(fun a -> a /. s)
;;

let divided s = Accessor.invert (multiplied s)
