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

(* The data generation functions require a source of bits. Either OCaml's
   [Random] module or a file can be used as a source. (When we are controlled
   by afl-fuzz, the latter mode is used.) *)

(* The function [with_source s f] makes the source [s] available during the
   execution of the function call [f()]. If [s] is [None], random data is
   used; if [s] is [Some filename], then input data is read from the file
   [filename]. In the latter case, any of the data generation functions can
   raise [Reject] at any time, as the input data file can be exhausted. *)

(* The channel through which input bits are read. *)

type source =
  | Closed
  | Random
  | Channel of in_channel

let source =
  ref Closed

let set_source s =
  match s with
  | None ->
      Printf.printf "No source file name specified; using random data.\n";
      source := Random
  | Some filename ->
      source := Channel (open_in filename)

let close_source () =
  match !source with
  | Closed ->
      ()
  | Random ->
      source := Closed
  | Channel c ->
      close_in_noerr c;
      source := Closed

let with_source s f =
  set_source s;
  let y = f() in
  close_source();
  y

(* -------------------------------------------------------------------------- *)

(* Generators. *)

type 'a gen =
  unit -> 'a

let byte () =
  (* This produces a byte viewed as an unsigned int. *)
  match !source with
  | Closed ->
      (* If we get here, then a data generation function is used
         outside of the data generation phase. *)
      assert false
  | Random ->
      Random.int 0x100
  | Channel c ->
      input_byte c
      (* Note: [End_of_file] can escape. *)

let short () =
  0x100 * byte() + byte()

let bits () =
  (* This produces a signed int. *)
  match !source with
  | Closed ->
      (* If we get here, then a data generation function is used
         outside of the data generation phase. *)
      assert false
  | Random ->
      Random.bits()
  | Channel c ->
      input_binary_int c
      (* Note: [End_of_file] can escape. *)

let bool () =
  let b = byte() in
  b land 1 = 1

let int n () =
  guard (0 < n);
  if n = 1 then
    (* Read 0 bits of input data. *)
    0
  else if n <= 0x100 then
    (* Read 8 bits of input data. *)
    byte() mod n
  else if n <= 0x10000 then
    (* Read 16 bits of input data. *)
    short() mod n
  else
    (* Read a word of input data. (Could do better.) *)
    abs (bits()) mod n

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

let interval i j () =
  if i < j then
    i + int (j - i) ()
  else
    reject()

let interval_ i j =
  interval i (j + 1)

let postincrement r =
  let x = !r in r := x + 1; x

let sequential () =
  let r = ref 0 in
  fun () ->
    postincrement r

let option element () =
  if bool() then
    None
  else
    Some (element())

(* A simplified version of [List.init]. *)
let rec init f accu k =
  if k = 0 then accu else init f (f() :: accu) (k - 1)

let list n element () =
  init element [] (n())

let array n element () =
  Array.init (n ()) (fun _i -> element())
