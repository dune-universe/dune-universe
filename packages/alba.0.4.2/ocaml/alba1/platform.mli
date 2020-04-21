(* Copyright (C) 2017 Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)


type t = <
    readdir: string -> string array;
    is_directory: string -> bool;
    mkdir: string -> int -> unit;
    getcwd: unit -> string;
    getenv: string -> string;
    path_separator: unit -> char;
    directory_separator: unit -> char;
    modification_time: string -> float
    >

val set: t -> unit
val get: unit -> t

val readdir: string -> string array
val path_exists: string -> bool
val is_directory: string -> bool
val mkdir: string -> int -> unit
val getcwd: unit -> string
val getenv: string -> string
val path_separator: unit -> char
val modification_time: string -> float
val write_dummy: string -> unit

val system: string -> Unix.process_status
val system_with_output: string -> string list

module Filename:
sig
  val concat: string -> string -> string
end
