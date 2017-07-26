open Stdint

type fuint16 = int64

let bit_mask = 0xFFFFL;;

let (&) (x:fuint16) (y:fuint16) =
  Int64.logand (Int64.logand x y) bit_mask
;;

let mask_to_uint16 (x:fuint16) =
  x & bit_mask
;;

let (or) (x:fuint16) (y:fuint16) =
  mask_to_uint16 (Int64.logor x y)
;;

let (^) (x:fuint16) (y:fuint16) =
  mask_to_uint16 (Int64.logxor x y)
;;

let (<<) (x:fuint16) (y:int) =
  mask_to_uint16 (Int64.shift_left x y)
;;

let (>>) (x:fuint16) (y:int) =
  mask_to_uint16 (Int64.shift_right_logical x y)
;;

let add1 (x:fuint16 ref) : unit =
  x := mask_to_uint16 (Int64.succ !x)
;;

let sub1 (x:fuint16 ref) : unit =
  x := mask_to_uint16 (Int64.pred !x)
;;

let to_int (x:fuint16) =
  Int64.to_int x
;;

let of_char (x:char) =
  mask_to_uint16 (Int64.of_int (Char.code x))
;;

let to_uint16 (x:fuint16) : uint16 =
  Uint16.of_int64 x
;;

let of_uint16 (x:uint16) : fuint16 =
  Uint16.to_int64 x
;;
