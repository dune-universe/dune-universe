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

(** Enum_info — Structs representing an enumeration and its values *)

open Ctypes

(** A Enum_info represents an enumeration and a Value_info struct represents
    a value of an enumeration. The Enum_info contains a set of values and a
    type The Value_info is fetched by calling Enum_info.get_value on a
    Enum_info.*)
type t
val enuminfo : t structure typ

(** Obtain the number of values this enumeration contains. *)
val get_n_values:
  t structure ptr -> int

(** Obtain the number of methods that this enum type has. *)
val get_n_methods:
  t structure ptr -> int

(** Obtain an enum type method at index n . *)
val get_method:
  t structure ptr -> int -> Function_info.t structure ptr

(** Obtain a value for this enumeration. *)
val get_value:
  t structure ptr -> int -> Value_info.t structure ptr option

(** Obtain the string form of the quark for the error domain associated with
    this enum, if any. *)
val get_error_domain:
  t structure ptr -> string option

(** Obtain the tag of the type used for the enum in the C ABI. This will
    be a signed or unsigned integral type.
    Note that in the current implementation the width of the type is computed
    correctly, but the signed or unsigned nature of the type may not match the
    sign of the type used by the C compiler. *)
val get_storage_type:
  t structure ptr -> Bindings.Types.tag

(** Just cast OCaml Ctypes base info to enum info. *)
val cast_from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes enum info to base info *)
val cast_to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Return a Enum_info.t from a Base_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Return a Base_info.t form a Enum_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Just cast OCaml Ctypes registeredtype info to enum info. *)
val cast_from_registeredtypeinfo:
  Registered_type_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes enum info to registeredtype info *)
val cast_to_registeredtypeinfo:
  t structure ptr -> Registered_type_info.t structure ptr

(** Return a Enum_info.t from a Registered_type_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Registered_type_info.registeredtypeinfo_unref. *)
val from_registeredtypeinfo:
  Registered_type_info.t structure ptr -> t structure ptr

(** Return a Registered_type_info.t form a Enum_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Registered_type_info.registeredtypeinfo_unref. *)
val to_registeredtypeinfo:
  t structure ptr -> Registered_type_info.t structure ptr
