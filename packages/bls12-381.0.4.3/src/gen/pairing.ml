(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
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

module type RAW_STUBS = sig
  val final_exponentiation : Bytes.t -> Bytes.t

  val miller_loop_simple : Bytes.t -> Bytes.t -> Bytes.t

  val pairing : Bytes.t -> Bytes.t -> Bytes.t

  val miller_loop_2 : Bytes.t -> Bytes.t -> Bytes.t -> Bytes.t -> Bytes.t

  val miller_loop_3 :
    Bytes.t -> Bytes.t -> Bytes.t -> Bytes.t -> Bytes.t -> Bytes.t -> Bytes.t

  val miller_loop_4 :
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t

  val miller_loop_5 :
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t

  val miller_loop_6 :
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t ->
    Bytes.t
end

module Make
    (G1 : G1.UNCOMPRESSED)
    (G2 : G2.UNCOMPRESSED)
    (GT : Fq12.T)
    (Stubs : RAW_STUBS) : sig
  exception FailToComputeFinalExponentiation of GT.t

  val miller_loop : (G1.t * G2.t) list -> GT.t

  (** Compute the miller loop on a single tuple of point *)
  val miller_loop_simple : G1.t -> G2.t -> GT.t

  (** Compute a pairing result of a list of points *)
  val pairing : G1.t -> G2.t -> GT.t

  (** Compute the final exponentiation of the given point. Returns a [None] if
      the point is null *)
  val final_exponentiation_opt : GT.t -> GT.t option

  (** Compute the final exponentiation of the given point. Raise
      [FailToComputeFinalExponentiation] if the point is null *)
  val final_exponentiation_exn : GT.t -> GT.t
end = struct
  module G1 = G1
  module G2 = G2
  module GT = GT

  exception FailToComputeFinalExponentiation of GT.t

  let miller_loop_simple (g1 : G1.t) (g2 : G2.t) : GT.t =
    let res = Stubs.miller_loop_simple (G1.to_bytes g1) (G2.to_bytes g2) in
    GT.of_bytes_exn res

  let pairing (g1 : G1.t) (g2 : G2.t) : GT.t =
    let res = Stubs.pairing (G1.to_bytes g1) (G2.to_bytes g2) in
    GT.of_bytes_exn res

  let final_exponentiation_exn (e : GT.t) : GT.t =
    if GT.is_zero e then raise (FailToComputeFinalExponentiation e)
    else
      let res = Stubs.final_exponentiation (GT.to_bytes e) in
      GT.of_bytes_exn res

  let final_exponentiation_opt (e : GT.t) : GT.t option =
    if GT.is_zero e then None
    else
      let res = Stubs.final_exponentiation (GT.to_bytes e) in
      GT.of_bytes_opt res

  let miller_loop (xs : (G1.t * G2.t) list) : GT.t =
    let rec f acc xs =
      match xs with
      | [] -> acc
      | [(g1, g2)] -> GT.mul (miller_loop_simple g1 g2) acc
      | [(g1_1, g2_1); (g1_2, g2_2)] ->
          let res =
            Stubs.miller_loop_2
              (G1.to_bytes g1_1)
              (G1.to_bytes g1_2)
              (G2.to_bytes g2_1)
              (G2.to_bytes g2_2)
          in
          GT.mul (GT.of_bytes_exn res) acc
      | [(g1_1, g2_1); (g1_2, g2_2); (g1_3, g2_3)] ->
          let res =
            Stubs.miller_loop_3
              (G1.to_bytes g1_1)
              (G1.to_bytes g1_2)
              (G1.to_bytes g1_3)
              (G2.to_bytes g2_1)
              (G2.to_bytes g2_2)
              (G2.to_bytes g2_3)
          in
          GT.mul (GT.of_bytes_exn res) acc
      | [(g1_1, g2_1); (g1_2, g2_2); (g1_3, g2_3); (g1_4, g2_4)] ->
          let res =
            Stubs.miller_loop_4
              (G1.to_bytes g1_1)
              (G1.to_bytes g1_2)
              (G1.to_bytes g1_3)
              (G1.to_bytes g1_4)
              (G2.to_bytes g2_1)
              (G2.to_bytes g2_2)
              (G2.to_bytes g2_3)
              (G2.to_bytes g2_4)
          in
          GT.mul (GT.of_bytes_exn res) acc
      | [(g1_1, g2_1); (g1_2, g2_2); (g1_3, g2_3); (g1_4, g2_4); (g1_5, g2_5)]
        ->
          let res =
            Stubs.miller_loop_5
              (G1.to_bytes g1_1)
              (G1.to_bytes g1_2)
              (G1.to_bytes g1_3)
              (G1.to_bytes g1_4)
              (G1.to_bytes g1_5)
              (G2.to_bytes g2_1)
              (G2.to_bytes g2_2)
              (G2.to_bytes g2_3)
              (G2.to_bytes g2_4)
              (G2.to_bytes g2_5)
          in
          GT.mul (GT.of_bytes_exn res) acc
      | (g1_1, g2_1)
        :: (g1_2, g2_2)
           :: (g1_3, g2_3) :: (g1_4, g2_4) :: (g1_5, g2_5) :: (g1_6, g2_6) :: xs
        ->
          let res =
            Stubs.miller_loop_6
              (G1.to_bytes g1_1)
              (G1.to_bytes g1_2)
              (G1.to_bytes g1_3)
              (G1.to_bytes g1_4)
              (G1.to_bytes g1_5)
              (G1.to_bytes g1_6)
              (G2.to_bytes g2_1)
              (G2.to_bytes g2_2)
              (G2.to_bytes g2_3)
              (G2.to_bytes g2_4)
              (G2.to_bytes g2_5)
              (G2.to_bytes g2_6)
          in
          let acc = GT.mul (GT.of_bytes_exn res) acc in
          f acc xs
    in
    if List.length xs = 0 then failwith "Empty list of points given"
    else f GT.one xs
end
