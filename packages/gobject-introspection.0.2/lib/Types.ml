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

open Ctypes
open Stubs

type argument_t
let argument : argument_t union typ = union "GIArgument"
let v_boolean = field argument "v_boolean" (bool)
let v_int8 = field argument "v_int8" (int8_t)
let v_uint8 = field argument "v_uint8" (uint8_t)
let v_int16 = field argument "v_int16" (int16_t)
let v_uint16 = field argument "v_uint16" (uint16_t)
let v_int32 = field argument "v_int32" (int32_t)
let v_uint32 = field argument "v_uint32" (uint32_t)
let v_int64 = field argument "v_int64" (int64_t)
let v_uint64 = field argument "v_uint64" (uint64_t)
let v_float = field argument "v_float" (float)
let v_double = field argument "v_double" (double)
let v_short = field argument "v_short" (short)
let v_ushort = field argument "v_ushort" (ushort)
let v_int = field argument "v_int" (int)
let v_uint = field argument "v_uint" (uint)
let v_long = field argument "v_long" (long)
let v_ulong = field argument "v_ulong" (ulong)
let v_ssize = field argument "v_ssize" (PosixTypes.ssize_t)
let v_size = field argument "v_size" (size_t)
let v_string = field argument "v_string" (string)
let v_pointer = field argument "v_pointer" (ptr_opt void)

let () = seal argument
