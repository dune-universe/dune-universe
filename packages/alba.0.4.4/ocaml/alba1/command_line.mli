(* Copyright (C) 2017 Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

type t

val command: t -> string list

val working_directory: t -> string

val verbosity: t -> int

val arguments: t -> string list

val package_paths: t -> string list

val is_forced: int -> t -> bool

val get: unit -> t
