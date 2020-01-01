(*
 * Copyright 2017-2019 Cedric LE MOIGNE, cedlemo@gmx.com
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

(** common types *)

open Ctypes
open Stubs

type argument_t
val argument: argument_t union typ
val v_boolean: (bool, argument_t union) field
val v_int8: (int, argument_t union) field
val v_uint8: (Unsigned.uint8, argument_t union) field
val v_int16: (int, argument_t union) field
val v_uint16: (Unsigned.uint16, argument_t union) field
val v_int32: (int32, argument_t union) field
val v_uint32: (Unsigned.uint32, argument_t union) field
val v_int64: (int64, argument_t union) field
val v_uint64: (Unsigned.uint64, argument_t union) field
val v_float: (float, argument_t union) field
val v_double: (float, argument_t union) field
val v_short: (int, argument_t union) field
val v_ushort: (Unsigned.ushort, argument_t union) field
val v_int: (int, argument_t union) field
val v_uint: (Unsigned.uint, argument_t union) field
val v_long: (Signed.long, argument_t union) field
val v_ulong: (Unsigned.ulong, argument_t union) field
val v_ssize: (PosixTypes.ssize_t, argument_t union) field
val v_size: (PosixTypes.size_t, argument_t union) field
val v_string: (string, argument_t union) field
val v_pointer: (unit Ctypes_static.ptr option, argument_t union) field

