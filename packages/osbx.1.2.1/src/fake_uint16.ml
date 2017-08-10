open Stdint

type fuint16 = int

let bit_mask = 0xFFFF;;

let (&) (x:fuint16) (y:fuint16) =
  (x land y) land bit_mask
;;

let mask_to_uint16 (x:fuint16) =
  x & bit_mask
;;

let (or) (x:fuint16) (y:fuint16) =
  mask_to_uint16 (x lor y)
;;

let (^) (x:fuint16) (y:fuint16) =
  mask_to_uint16 (x lxor y)
;;

let (<<) (x:fuint16) (y:int) =
  mask_to_uint16 (x lsl y)
;;

let (>>) (x:fuint16) (y:int) =
  mask_to_uint16 (x lsr y)
;;

let add1 (x:fuint16 ref) : unit =
  x := mask_to_uint16 (succ !x)
;;

let sub1 (x:fuint16 ref) : unit =
  x := mask_to_uint16 (pred !x)
;;

let of_int (x:int) : fuint16 =
  x
;;

let to_int (x:fuint16) : int =
  x
;;

let of_char (x:char) =
  mask_to_uint16 (Char.code x)
;;

let to_uint16 (x:fuint16) : uint16 =
  Uint16.of_int x
;;

let of_uint16 (x:uint16) : fuint16 =
  Uint16.to_int x
;;
