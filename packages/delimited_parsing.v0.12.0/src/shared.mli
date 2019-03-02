open! Core
open! Async

(** Kitchen-sink module for functions shared by multiple parsers. *)

include module type of Delimited_kernel.Shared

(** [drop_lines r n] reads and discards up to and including the [n]th newline from [r]. *)
val drop_lines : Reader.t -> int -> unit Deferred.t
