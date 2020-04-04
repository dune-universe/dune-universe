(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

(* This module offers a few facilities that help with fuzzing. *)

(* -------------------------------------------------------------------------- *)

(* The exception [Reject] is raised to indicate that a wrong choice was made
   during data generation, so this test should be silently stopped this test.
   We do not provide any functions that handle this exception; it is up to the
   user to do so. *)

exception Reject

(* [reject()] unconditionally raises [Reject]. *)

val reject: unit -> 'a

(* [guard b] raises [Reject] if [b] is false. *)

val guard: bool -> unit

(* -------------------------------------------------------------------------- *)

(* The exception [Fail] is raised to indicate that a test fails. Any exception
   can in fact be used for this purpose. The only way in which [Fail] receives
   special treatment is that a backtrace is *not* printed when this exception
   is detected. Only the error message carried by this exception is printed. *)

exception Fail of string

(* [fail format ...] raises a [Fail] exception. Like [sprintf], it takes a
   variable list of arguments. *)

val fail: ('a, unit, string, 'b) format4 -> 'a

(* [display_failure_and_abort e] displays the exception [e] and aborts the
   whole process. *)

val display_failure_and_abort: exn -> 'a

(* -------------------------------------------------------------------------- *)

(* Data generation. *)

(* The following data generation functions require a source of bits. Either
   OCaml's [Random] module or a file can be used as a source. The function
   [with_source s f] makes the source [s] available during the execution of
   the function call [f()]. If [s] is [None], random data is used; if [s] is
   [Some filename], then input data is read from the file [filename]. In the
   latter case, any of the data generation functions can raise [Reject] at any
   time, as the input data file can be exhausted. *)

val with_source: string option -> (unit -> 'b) -> 'b

(* The type of a generator. *)

type 'a gen =
  unit -> 'a

(* [byte()] generates a byte. A byte is viewed as an unsigned integer. *)

val byte: int gen

(* [bits()] generates a signed integer. *)

val bits: int gen

(* [bool()] generates a Boolean. *)

val bool: bool gen

(* [int n] generates an integer in the range [0..n). The parameter [n]
   must be strictly positive, otherwise [Reject] is raised. *)

val int: int -> int

(* [choose xs] picks an element in the list [xs]. The list must be nonempty,
   otherwise [Reject] is raised. *)

val choose: 'a list -> 'a
