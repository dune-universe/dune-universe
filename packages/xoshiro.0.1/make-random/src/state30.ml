(** {1 State30}

   This module contains one unique functor which, provided a type [state] and a
   function [bits] generates bits from the state, returns a module similar to
   that of {!Stdlib.Random.State}, containing all the functions that take the
   state and generate bits from it.

   This is the 30-bits version: the function [bits] is expected to return an
   [int] whose 30 lower bits {b only} are set. *)

module Make (B : Bits.State30) = struct
  let bits = B.bits

  (* The following is copied from Stdlib.Random.State.

     Copyright 1996 Institut National de Recherche en Informatique et en
     Automatique. *)

  let rec intaux s n =
    let r = bits s in
    let v = r mod n in
    if r - v > 0x3FFFFFFF - n + 1 then intaux s n else v

  let int s bound =
    if bound > 0x3FFFFFFF || bound <= 0
    then invalid_arg "Random.int"
    else intaux s bound

  let rec int32aux s n =
    let b1 = Int32.of_int (bits s) in
    let b2 = Int32.shift_left (Int32.of_int (bits s land 1)) 30 in
    let r = Int32.logor b1 b2 in
    let v = Int32.rem r n in
    if Int32.sub r v > Int32.add (Int32.sub Int32.max_int n) 1l
    then int32aux s n
    else v

  let int32 s bound =
    if bound <= 0l
    then invalid_arg "Random.int32"
    else int32aux s bound

  let rec int64aux s n =
    let b1 = Int64.of_int (bits s) in
    let b2 = Int64.shift_left (Int64.of_int (bits s)) 30 in
    let b3 = Int64.shift_left (Int64.of_int (bits s land 7)) 60 in
    let r = Int64.logor b1 (Int64.logor b2 b3) in
    let v = Int64.rem r n in
    if Int64.sub r v > Int64.add (Int64.sub Int64.max_int n) 1L
    then int64aux s n
    else v

  let int64 s bound =
    if bound <= 0L
    then invalid_arg "Random.int64"
    else int64aux s bound

  let nativeint =
    if Nativeint.size = 32
    then fun s bound -> Nativeint.of_int32 (int32 s (Nativeint.to_int32 bound))
    else fun s bound -> Int64.to_nativeint (int64 s (Int64.of_nativeint bound))

  let rawfloat s =
    let scale = 1073741824.0  (* 2^30 *)
    and r1 = float_of_int (bits s) (* note: in original file, Stdlib.float is used *)
    and r2 = float_of_int (bits s) (* note: in original file, Stdlib.float is used *)
    in (r1 /. scale +. r2) /. scale

  let float s bound = rawfloat s *. bound

  let bool s = (bits s land 1 = 0)
end
