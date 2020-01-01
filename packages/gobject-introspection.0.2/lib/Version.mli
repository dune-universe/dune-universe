(*
 * Copyright 2019 Cedric LE MOIGNE, cedlemo@gmx.com
 * This file is part of OCaml-GObject-Introspection.
 *
 * OCaml-GObject-Introspection is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * OCaml-GObject-Introspection is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-GObject-Introspection.  If not, see <http://www.gnu.org/licenses/>.
 *)

(**
 * Returns the major version number of the girepository library. (e.g. in version 1.58.2 this is 1.)
 * *)
val get_major_version : unit -> int

(**
 * Returns the minor version number of the girepository library. (e.g. in version 1.58.2 this is 58.)
 * *)
val get_minor_version : unit -> int

(**
 * Returns the micro version number of the girepository library. (e.g. in version 1.58.2 this is 2.)
 * *)
val get_micro_version : unit -> int
