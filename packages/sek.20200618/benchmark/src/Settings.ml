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

open Printf
open Shared

(* If [-dry-run true] is specified, we are supposed to run the
   benchmark just once and output its result. *)

let dry =
  Cmdline.parse_or_default_bool "dry-run" false

(* This sink is designed to ensure that a result appears to be
   used, so the compiler cannot optimize away its computation. *)

let null =
  if dry then stdout else open_out "/dev/null"

let sink (m : int) =
  fprintf null "%d\n" m

(* Random seed. *)

let () =
  let seed = Cmdline.parse_or_default_int "seed" 1 in
   Random.init seed

(* GC settings. *)

let minor_heap_multiple_of_32k =
  Cmdline.parse_or_default_int "minor_heap_multiple_of_32k" 32 (* 1MB *)

let () =
  let minor_heap_size = 32768 * minor_heap_multiple_of_32k in
  printf "minor_heap_size_kb %d\n" (minor_heap_size / 1024);
  Gc.set {(Gc.get ()) with Gc.minor_heap_size = minor_heap_size }

(* Check that Sek has been compiled in release mode. *)

let () =
  Sek.released()

(* Settings for Sek. *)

module Settings = struct

  include Sek.DefaultSettings

  let chunk_capacity =
    Cmdline.parse_or_default_int "chunk_capacity" 128

  let deep_chunk_capacity =
    Cmdline.parse_or_default_int "deep_chunk_capacity" 16

  let capacity depth =
    if depth = 0 then chunk_capacity else deep_chunk_capacity

  let overwrite_empty_slots =
    Cmdline.parse_or_default_bool "overwrite_empty_slots" true

  let check_iterator_validity =
    Cmdline.parse_or_default_bool "check_iterator_validity" true

end

module CustomSek =
  Sek.Make(Settings)
