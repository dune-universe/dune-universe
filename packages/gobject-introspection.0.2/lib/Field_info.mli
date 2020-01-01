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

(** Field_info — Struct representing a struct or union field *)

open Ctypes

(** A Field_info struct represents a field of a struct (see Struct_info),
    union (see Union_info) or an object (see Object_info).
    The Field_info is fetched by calling
    Struct_info.get_field,
    Union_info.get_field
    or Object_info.get_field. A field has a size, type and a struct offset
    asssociated and a set of flags, which is currently
    GI_FIELD_IS_READABLE or GI_FIELD_IS_WRITABLE.*)
type t
val fieldinfo : t structure typ

(** Obtain the flags for this Field_info. See Field_info.flags for possible
    flag values. *)
val get_flags:
  t structure ptr -> Bindings.Field_info.flags list

(** Obtain the offset in bits of the field member, this is relative to the
    beginning of the struct or union. *)
val get_offset:
  t structure ptr -> int

(** Obtain the size in bits of the field member, this is how much space you
    need to allocate to store the field. *)
val get_size:
  t structure ptr -> int

(** Obtain the type of a field as a Type_info. *)
val get_type:
  t structure ptr -> Type_info.t structure ptr

(*
   gboolean	g_field_info_get_field ()
   gboolean	g_field_info_set_field ()
 *)

(** Just cast OCaml Ctypes base info to field info. *)
val cast_from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes field info to base info *)
val cast_to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Add unref of the C underlying structure whith Gc.finalise. *)
val add_unref_finaliser:
  t structure ptr -> t structure ptr

(** Return a Field_info.t from a Base_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Return a Base_info.t from a Field_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val to_baseinfo:
  t structure ptr -> Base_info.t structure ptr
