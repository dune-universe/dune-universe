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

(* This module offers facilities for generating values of various types,
   based on data that is read from our standard input channel (which is
   controlled by afl-fuzz). *)

open Misc
open Error

(* -------------------------------------------------------------------------- *)

(* The exception [Reject] is raised to indicate that a wrong choice was made
   during data generation, so this test should be silently stopped. E.g.,
   attempting to choose an object from an empty list raises [Reject]. *)

exception Reject

let reject () =
  raise Reject

let guard b =
  if not b then reject()

(* -------------------------------------------------------------------------- *)

(* The data generation functions require a source of bits, which is internally
   represented as an input channel. *)

(* The function [with_source s f] makes the source [s] available during the
   execution of the function call [f()]. If [s] is [None], random data is
   used; if [s] is [Some filename], then input data is read from the file
   [filename]. In the latter case, any of the data generation functions can
   raise [Reject] at any time, as the input data file can be exhausted. *)

(* The channel through which input bits are read. *)

let source =
  ref stdin

let set_source s =
  match s with
  | None ->
      source := open_in "/dev/urandom"
  | Some filename ->
      source := open_in filename

let close_source () =
  close_in_noerr !source

let with_source s f =
  set_source s;
  let y = f() in
  close_source();
  y

(* -------------------------------------------------------------------------- *)

(* Generators. *)

type 'a gen =
  unit -> 'a

exception OutOfInputData

(* [wrap f x] invokes [f x], which is expected to read some data from
   the input channel, and handles the exceptions that might result. *)

let wrap f x =
  try
    f x
  with
  | End_of_file ->
      (* The input stream has been exhausted. We remap to a different
         exception so as to avoid any confusion. *)
      raise OutOfInputData
  | Sys_error _ ->
      (* The source channel seems to have been closed. It is likely that
         a data generation function has been invoked outside of the data
         generation phase. *)
      error "Monolith.Gen.* cannot be used outside of Monolith.main."

(* [byte()] produces an unsigned 8-bit integer. *)

let byte () =
  wrap input_byte !source

(* [short()] produces an unsigned 16-bit integer. *)

let short () =
  byte() lsl 8 lor byte()

(* [long()] produces an unsigned 32-bit integer. *)

let long () =
  assert (Sys.word_size > 32);
  short() lsl 16 lor short()

(* [signed_long()] produces a signed 32-bit integer. *)

(* On a 32-bit machine, I expect that we get only 31 bits. *)

let signed_long () =
  wrap input_binary_int !source

(* [bits()] produces a signed integer. All values of type [int] can in
   principle be produced. *)

let bits63 () =
  (signed_long() lsl 31) lor (signed_long() land (1 lsl 32 - 1))

let bits () =
  match Sys.word_size with
  | 32 ->
      signed_long()
  | 64 ->
      bits63()
  | _ ->
      assert false

(* [bool()] produces a Boolean. *)

let bool () =
  let b = byte() in
  b land 1 = 1

(* [char()] produces a character. *)

let char () =
  let b = byte() in
  Char.chr b

(* [log2 n] is the base two logarithm of [n]. *)

let rec log2 accu n =
  if n = 1 then
    accu
  else
    log2 (accu + 1) (n lsr 1)

let log2 n =
  assert (0 < n);
  log2 0 n

(* [next_power_of_two n] is the smallest power of two that is strictly
   greater than [n]. *)

let next_power_of_two n =
  1 lsl (log2 n + 1)

(* [mask n] is a mask (a sequence of "1" bits), and it is the smallest
   mask such that [n land (mask n)] is [n]. *)

let mask n =
  next_power_of_two n - 1

(* [truncate n i] truncates the integer [i], which may be larger than [n],
   so that it fits in the semi-open interval [0, n). *)

(* This could be done just by computing [i mod n]. However, it is perhaps
   preferable to avoid depending on the most significant bits of [i]; this may
   help afl-fuzz detect that these bits are irrelevant. For this reason, we
   first perform a logical AND against [mask n]. *)

let truncate n i =
  assert (0 < n);
  assert (0 <= i);
  (i land (mask n)) mod n

let int n () =
  guard (0 < n);
  if n = 1 then
    (* Read 0 bits of input data. *)
    0
  else if n <= 1 lsl 8 then
    (* Read 8 bits of input data. *)
    byte() |> truncate n
  else if n <= 1 lsl 16 then
    (* Read 16 bits of input data. *)
    short() |> truncate n
  else if n <= 1 lsl 32 then
    long() |> truncate n
  else begin
    assert (Sys.word_size = 64);
    (bits63() land max_int) |> truncate n
  end

let choose xs =
  let xs = Array.of_list xs in
  fun () ->
    let n = Array.length xs in
    let i = int n () in
    xs.(i)

let lt j =
  int j

let le j =
  int (j + 1)

let semi_open_interval i j () =
  if i < j then begin
    assert (0 < j - i); (* protect against overflow *)
    i + int (j - i) ()
  end
  else
    reject()

(* We do not define [closed_interval i j] as [semi_open_interval i (j + 1)]
   because this definition does not work when [j] is [max_int]. *)

let closed_interval i j () =
  if i <= j then begin
    assert (0 <= j - i && j - i < max_int); (* protect against overflow *)
    i + int (j - i + 1) ()
  end
  else
    reject()

let sequential () =
  (* The reference that is allocated here is a piece of state that may need to
     be reset at the beginning of every run, so that all runs begin with the
     same initial state. Thus, it must be declared. We do not know whether
     [GlobalState.save] has been called already, or has not been called yet.
     We exploit the fact that it is permitted to call [GlobalState.register]
     at any time. *)
  let r = ref 0 in
  GlobalState.register_ref r;
  fun () ->
    postincrement r

let option element () =
  if bool() then
    None
  else
    Some (element())

let result left right () =
  if bool() then
    Ok (left())
  else
    Error (right())

(* A simplified version of [List.init]. *)
let rec init f accu k =
  if k = 0 then accu else init f (f() :: accu) (k - 1)

let list n element () =
  init element [] (n())

let array n element () =
  Array.init (n ()) (fun _i -> element())

let string n char () =
  String.init (n ()) (fun _i -> char())
