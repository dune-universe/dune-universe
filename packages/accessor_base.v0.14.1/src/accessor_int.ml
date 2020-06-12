open! Base
open! Import

let negated = [%accessor Accessor.isomorphism ~get:Int.neg ~construct:Int.neg]
let added s = Accessor.isomorphism ~get:(fun a -> a + s) ~construct:(fun a -> a - s)
let subtracted s = Accessor.invert (added s)
let incremented = [%accessor Accessor.isomorphism ~get:Int.succ ~construct:Int.pred]
let decremented = [%accessor Accessor.invert incremented]

let bit_at_exn i =
  if i < 0
  then raise_s [%message "Accessor.Int.bit_at_exn: negative index" (i : int)]
  else if i >= Int.num_bits
  then
    raise_s
      [%message
        "Accessor.Int.bit_at_exn: index too large" (i : int) (Int.num_bits : int)]
  else (
    let mask = 1 lsl i in
    Accessor.field
      ~get:(fun int -> int land mask <> 0)
      ~set:(fun int bit -> if bit then int lor mask else int land lnot mask))
;;

let bit_negated = [%accessor Accessor.isomorphism ~get:lnot ~construct:lnot]
let bit_xored x = Accessor.isomorphism ~get:(Int.bit_xor x) ~construct:(Int.bit_xor x)
