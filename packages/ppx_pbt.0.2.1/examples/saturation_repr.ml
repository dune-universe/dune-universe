(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Saturation_repr is a representation of saturation arithmetic. The
    type Saturation_repr.t must be inside [0..max_int].

    It includes multiple arithmetic properties tested here with ppx_pbt.

    capped f saturated:
      forall x,
        f saturated x = saturated &&
        f x saturated = saturated
    
    floored f floor:
      forall x,
        f floor x = floor &&
        f x floor = floor

    absorbs f absorb:
      forall x,
        f absorb x = absorb &&
        f x absorb = absorb

    commutative f:
      forall x y,
        f x y = f y x

    associative f:
      forall x y z,
        f x (f y z) = f (f x y) z

    oracle f oracle:
      forall x y,
        f x y = oracle x y
*)

let uint = QCheck.(map abs int)

module Int = struct
  type t = int

  external ( = ) : int -> int -> bool = "%equal"

  external ( <> ) : int -> int -> bool = "%notequal"

  external ( < ) : int -> int -> bool = "%lessthan"

  external ( > ) : int -> int -> bool = "%greaterthan"

  external ( <= ) : int -> int -> bool = "%lessequal"

  external ( >= ) : int -> int -> bool = "%greaterequal"

  external compare : int -> int -> int = "%compare"

  let equal = ( = )

  let max x y = if x >= y then x else y

  let min x y = if x <= y then x else y
end

let decimals = 3

type fp_tag

type integral_tag

module Saturating_repr = struct
  type _ t = int

  type mul_safe

  type may_saturate

  let zero = 0

  let small_enough z =
    (* The following literal triggers an error if compiled under 32-bit
       architectures, please do not modify it. This is a static way to
       ensure that this file is compiled under a 64-bit architecture. *)
    z land 0x7fffffff80000000 = 0

  let saturated = max_int

  let of_int_opt t = if t >= 0 && t < saturated then Some t else None

  let mul x y =
    (* assert (x >= 0 && y >= 0); *)
    match x with
    | 0 -> 0
    | x ->
        if small_enough x && small_enough y then x * y
        else if Int.(y > saturated / x) then saturated
        else x * y
    [@@pbt
      {| capped{saturated}[uint];
         floored{zero}[uint];
         absorbs{zero}[uint];
         commutative[uint, uint];
         associative[uint, uint, uint]
    |}]

  let mul_fast x y =
    x * y
    [@@pbt
      {| absorbs{zero}[uint];
         commutative[uint, uint];
         associative[uint, uint, uint]
    |}]

  let scale_fast x y =
    if x = 0 then 0
    else if small_enough y then x * y
    else if Int.(y > saturated / x) then saturated
    else x * y
    [@@pbt {| oracle{mul}[uint, uint] |}]

  let add x y =
    let z = x + y in
    if z >= 0 then z else saturated
    [@@pbt
      {| capped{saturated}[uint];
         commutative[uint, uint];
         associative[uint, uint, uint]
    |}]

  let sub x y = Int.max (x - y) 0 [@@pbt {| floored_left{zero}[uint] |}]

  let sub_opt x y =
    let s = x - y in
    if Int.(s >= 0) then Some s else None
end
