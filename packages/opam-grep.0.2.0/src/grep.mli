(* SPDX-License-Identifier: MIT *)

exception OpamGrepError of string

val search : regexp:string -> unit
