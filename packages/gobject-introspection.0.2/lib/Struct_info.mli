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

(** Struct_info — Module representing a C structure *)

open Ctypes

(** Struct_info represents a generic C structure type.
 A structure has methods and fields.*)
type t
val structinfo : t structure typ

(** Return true if this structure represents the "class structure" for some
    GObject or GInterface. This function is mainly useful to hide this kind of
    structure from generated public APIs.
*)
val is_gtype_struct:
  t structure ptr -> bool

(** Obtain the required alignment of the structure.*)
val get_alignment:
  t structure ptr -> int

(** Obtain the total size of the structure. *)
val get_size:
  t structure ptr -> int

(** No doc yet *)
val is_foreign:
  t structure ptr -> bool

(** Obtain the number of fields this structure has. *)
val get_n_fields:
  t structure ptr -> int

(** Obtain the number of methods this structure has. *)
val get_n_methods:
  t structure ptr -> int

(** Obtain the type information for field with specified index. *)
val get_field:
  t structure ptr -> int -> Field_info.t structure ptr

(** Obtain the type information for method with specified index. *)
val get_method:
  t structure ptr -> int -> Function_info.t structure ptr

(** Obtain the type information for method named name .*)
val find_method:
  t structure ptr -> string -> Function_info.t structure ptr option

(** Add unref of the C underlying structure whith Gc.finalise. *)
val add_unref_finaliser:
  t structure ptr -> t structure ptr

(** Return a Struct_info.t from a Base_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Return a Base_info.t form a Struct_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Just cast OCaml Ctypes registeredtype info to struct info. *)
val cast_from_registeredtypeinfo:
  Registered_type_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes struct info to registeredtype info *)
val cast_to_registeredtypeinfo:
  t structure ptr -> Registered_type_info.t structure ptr

(** Return a Struct_info.t from a Registered_type_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Registered_type_info.registeredtypeinfo_unref. *)
val from_registeredtypeinfo:
  Registered_type_info.t structure ptr -> t structure ptr

(** Return a Registered_type_info.t form a Struct_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Registered_type_info.registeredtypeinfo_unref. *)
val to_registeredtypeinfo:
  t structure ptr -> Registered_type_info.t structure ptr
