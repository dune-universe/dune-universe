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

(** Type_info — Struct representing a type *)

open Ctypes

(** Type_info represents a type. You can retrieve a type info from an argument
    (see Arg_info), a function return value (see Function_info), a field
    (see Field_info), a property (see Property_info), a constant
    (see Constant_info) or for a union discriminator (see Union_info).
    A type can either be a of a basic type which is a standard C primitive type
    or an interface type. For interface types you need to call
    Type_info.get_interface to get a reference to the base info for that
    interface. *)
type t
val typeinfo : t structure typ

(** Obtain a string representation of type. *)
val to_string:
  t structure ptr -> string

(** Obtain if the type is passed as a reference.
    Note that the types of GI_DIRECTION_OUT and GI_DIRECTION_INOUT parameters
    will only be pointers if the underlying type being transferred is a pointer
    (i.e. only if the type of the C function’s formal parameter is a pointer to
    a pointer). *)
val is_pointer:
  t structure ptr -> bool

(** Obtain the type tag for the type. See GITypeTag for a list of type tags. *)
val get_tag:
  t structure ptr -> Bindings.Types.tag

(** Obtain the array length of the type. The type tag must be a GIType.array or
    -1 will returned. *)
val get_array_length:
  t structure ptr -> int

(** Obtain the fixed array size of the type. The type tag must be a
    GIType.array or -1 will returned. *)
val get_array_fixed_size:
  t structure ptr -> int

(** Obtain if the last element of the array is NULL. The type tag must be a
    GIType.array  or FALSE will returned. *)
val is_zero_terminated:
  t structure ptr -> bool

(** Obtain the array type for this type. See GIArrayType for a list of possible
    values. If the type tag of this type is not array, None will be returned. *)
val get_array_type:
  t structure ptr -> Bindings.Types.array_type option

(** Obtain the parameter type n. It is used to get the parameter type of generic
 *  container. For example if the current Type_info.t is an array, a GList, a
 *  GSList or an Hash, you have to use this in order to get the Type_info.t of
 *  the data of the container.
 *  Basicaly it is done with Type_info.get_param_type type_info 0 .
 *  https://github.com/GNOME/pygobject/blob/288bd2957916568666c3d5d27e8c868d67b7e8c9/gi/pygi-argument.c#L526
 *  https://github.com/ruby-gnome2/ruby-gnome2/blob/ec4160b4f4c5432c6ff8a9e962e7ffeacfd48869/gobject-introspection/ext/gobject-introspection/rb-gi-argument.c#L928
 *  This function is unsafe meanings that it is your responsability to not use
 *  a number out of bounds.
 *)
val unsafe_get_param_type:
  t structure ptr -> int -> t structure ptr

(** This is just an alias to Type_info.unsage_get_param_type with the index set
    to 0.
*)
val get_param_type:
  t structure ptr -> t structure ptr

(** For types which have Types.Interface such as GObjects and boxed
    values, this function returns full information about the referenced type.
    You can then inspect the type of the returned Base_info to further query
    whether it is a concrete GObject, a GInterface, a structure, etc. using
    Base_info.get_type. *)
val get_interface:
  t structure ptr -> Base_info.t structure ptr option

(** Just cast OCaml Ctypes base info to typeinfo. *)
val cast_from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes typeinfo to base info *)
val cast_to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Add unref of the C underlying structure whith Gc.finalise. *)
val add_unref_finaliser:
  t structure ptr -> t structure ptr

(** Return a Type_info.t from a Base_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Return a Base_info.t form a Type_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val to_baseinfo:
  t structure ptr -> Base_info.t structure ptr
