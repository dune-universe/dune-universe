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

(* Turn on the recording of backtraces, so an uncaught exception causes a
   backtrace to be printed before the process is aborted. *)

let () =
  Printexc.record_backtrace true

(* -------------------------------------------------------------------------- *)

(* [abort()] aborts the process by sending a SIGABRT signal to itself. We
   seldom need to use it directly, as letting an uncaught exception escape has
   the same effect. We use it only when we wish to abort without displaying a
   backtrace. *)

let abort () =
  flush stdout;
  flush stderr;
  let self = Unix.getpid() in
  Unix.kill self Sys.sigabrt;
  (* This point should be unreachable. *)
  assert false

(* -------------------------------------------------------------------------- *)

(* The exception [Reject] is raised to indicate that a wrong choice was made
   during data generation, so this test should be silently stopped. We do not
   provide any functions that handle this exception; it is up to the user to
   do so. *)

exception Reject

let reject () =
  raise Reject

let guard b =
  if not b then reject()

(* -------------------------------------------------------------------------- *)

(* The exception [Fail] is raised to indicate that a test fails. Any exception
   can in fact be used for this purpose. The only way in which [Fail] receives
   special treatment is that a backtrace is *not* printed when this exception
   is detected. Only the error message carried by this exception is printed. *)

exception Fail of string

let _fail s =
  raise (Fail s)

let fail format =
  Printf.ksprintf _fail format

let display_failure_and_abort e =
  match e with
  | Fail msg ->
      (* If this is a [Fail] exception, print just the message it carries. *)
      Printf.printf "%s.\n" msg;
      abort()
  | e ->
      (* Otherwise, print a full backtrace. *)
      raise e

(* -------------------------------------------------------------------------- *)

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
      try
        input_byte c
      with End_of_file ->
        reject()

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
      try
        input_binary_int c
      with End_of_file ->
        reject()

let bool () =
  let b = byte() in
  b land 1 = 1

let int n =
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
  let n = List.length xs in
  let i = int n in
  List.nth xs i
