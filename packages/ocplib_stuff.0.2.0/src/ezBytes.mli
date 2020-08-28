(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open [@ocaml.warning "-33"] EzCompat

(* [alloc size] returns a [bytes] values of size [size], which must be
   a power of 2 between 4kB and 1MB. The value might be a fresh bytes
   or a reused one. *)
val alloc : int -> Bytes.t

(* [free b] indicates that [b] is not used anymore, and should be
   stored for reuse. *)
val free : bytes -> unit

(* [set_max_queue_size n] will prevent to keep more than [n] buffers
   for reuse. If a queue is bigger, it is immediately cleaned.
   Default is 100. *)
val set_max_queue_size : int -> unit
