(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

(* A reference implementation of a nondeterministic operation that is
   supposed to return a pair of an even number and an odd number. *)

open Monolith
open Monolith.Print
open PPrint

let must_have_parity (parity : int) (candidate : int) : int diagnostic =
  if candidate mod 2 <> parity then
    Invalid (fun observed ->
      assert_ (observed ^^ utf8format " mod 2 = %d" parity) ^^ string ";;" ^^
      candidate_finds (int candidate)
    )
  else
    Valid candidate

let must_be_even : int -> int diagnostic =
  must_have_parity 0

let must_be_odd : int -> int diagnostic =
  must_have_parity 1

let f (_ : int) =
  must_be_even, must_be_odd
