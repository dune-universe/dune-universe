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

(** Object_info — Struct representing a GObject *)

open Ctypes

(** Object_info represents a GObject. This doesn't represent a specific
    instance of a GObject, instead this represent the object type (eg class).
    A GObject has methods, fields, properties, signals, interfaces, constants
    and virtual functions.*)
type t
val objectinfo : t structure typ

(** Obtain if the object type is an abstract type, eg if it cannot be
    instantiated *)
val get_abstract:
  t structure ptr -> bool

(** Obtain if the object type is of a fundamental type which is not
    G_TYPE_OBJECT. This is mostly for supporting GstMiniObject. *)
val get_fundamental:
  t structure ptr -> bool

(** Obtain the parent of the object type.*)
val get_parent:
  t structure ptr -> Base_info.t structure ptr

(** Obtain the name of the objects class/type. *)
val get_type_name:
  t structure ptr -> string

(** Obtain the function which when called will return the GType function for
    which this object type is registered. *)
val get_type_init:
  t structure ptr -> string

(** Obtain the number of constants that this object type has. *)
val get_n_constants:
  t structure ptr -> int

(** Obtain an object type constant at index n . *)
val get_constant:
  t structure ptr -> int -> Constant_info.t structure ptr

(** Obtain the number of fields that this object type has. *)
val get_n_fields:
  t structure ptr -> int

(** Obtain an object type field at index n . *)
val get_field:
  t structure ptr -> int -> Field_info.t structure ptr

(** Obtain the number of interfaces that this object type has. *)
val get_n_interfaces:
  t structure ptr -> int

(** Obtain an object type interface at index n . *)
val get_interface:
  t structure ptr -> int -> Interface_info.t structure ptr

(** Obtain the number of methods that this object type has. *)
val get_n_methods:
  t structure ptr -> int

(** Obtain an object type method at index n . *)
val get_method:
  t structure ptr -> int -> Function_info.t structure ptr

(** Obtain a method of the object type given a name . None will be returned if
    there's no method available with that name. *)
val find_method:
  t structure ptr -> string -> Function_info.t structure ptr option

(** Obtain the number of properties that this object type has. *)
val get_n_properties:
  t structure ptr -> int

(** Obtain an object type property at index n .*)
val get_property:
  t structure ptr -> int -> Property_info.t structure ptr

(** Obtain the number of signals that this object type has. *)
val get_n_signals:
  t structure ptr -> int

(** Obtain an object type signal at index n .*)
val get_signal:
  t structure ptr -> int -> Signal_info.t structure ptr

(** Find a signal with a name. *)
val find_signal:
  t structure ptr -> string -> Signal_info.t structure ptr option

(** Obtain the number of virtual functions that this object type has. *)
val get_n_vfuncs:
  t structure ptr -> int

(** Obtain an object type virtual function at index n . *)
val get_vfunc:
  t structure ptr -> int -> VFunc_info.t structure ptr

(** Locate a virtual function slot with name name . Note that the namespace for
    virtuals is distinct from that of methods; there may or may not be a
    concrete method associated for a virtual. If there is one, it may be
    retrieved using VFunc_info.get_invoker, otherwise None will be returned.
    See the documentation for VFunc_info.get_invoker for more information on
    invoking virtuals. *)
val find_vfunc:
  t structure ptr -> string -> VFunc_info.t structure ptr option

(** Every GObject has two structures; an instance structure and a class
    structure. This function returns the metadata for the class structure.
    It returns a Struct_info.t or None. *)
val get_class_struct:
  t structure ptr -> Struct_info.t structure ptr option

(** Obtain a method of the object given a name , searching both the object info
    and any interfaces it implements. None will be returned if there's no
    method available with that name.
    Note that this function does *not* search parent classes; you will have to
    chain up if that's desired. *)
val find_method_using_interfaces:
  t structure ptr -> string -> (Function_info.t structure ptr option *
                                t structure ptr option)

(** Obtain the symbol name of the function that should be called to ref this
    object type. It's mainly used fundamental types. The type signature for the
    symbol is Object_infoRefFunction, to fetch the function pointer
    see Object_info.get_ref_function. *)
val get_ref_function:
  t structure ptr -> string option

(** Obtain the symbol name of the function that should be called to unref this
    object type. It's mainly used fundamental types. The type signature for the
    symbol is Object_infoUnrefFunction, to fetch the function pointer see
    Object_info.get_unref_function. *)
val get_unref_function:
  t structure ptr -> string option

(** Obtain the symbol name of the function that should be called to convert set
    a GValue giving an object instance pointer of this object type. I's mainly
    used fundamental types. The type signature for the symbol is
    Object_infoSetValueFunction, to fetch the function pointer see
    Object_info.get_set_value_function. *)
val get_set_value_function:
  t structure ptr -> string option

(** Obtain the symbol name of the function that should be called to convert an
    object instance pointer of this object type to a GValue. I's mainly used
    fundamental types. The type signature for the symbol is
    Object_infoGetValueFunction, to fetch the function pointer see
    Object_info.get_get_value_function. *)
val get_get_value_function:
  t structure ptr -> string option

(** Locate a virtual function slot with name name , searching both the object
    info and any interfaces it implements. Note that the namespace for virtuals
    is distinct from that of methods; there may or may not be a concrete method
    associated for a virtual. If there is one, it may be retrieved using
    g_vfunc_info_get_invoker(), otherwise None will be returned.
    Note that this function does *not* search parent classes; you will have to
    chain up if that's desired. *)
val find_vfunc_using_interfaces:
  t structure ptr -> string -> (VFunc_info.t structure ptr option *
                                t structure ptr option)

(*
  TODO : Object_infoRefFunction	g_object_info_get_ref_function_pointer ()
  TODO : Object_infoUnrefFunction	g_object_info_get_unref_function_pointer ()
  TODO : Object_infoSetValueFunction	g_object_info_get_set_value_function_pointer ()
  TODO : Object_infoGetValueFunction	g_object_info_get_get_value_function_pointer ()
*)

(** Just cast OCaml Ctypes base info to object info. *)
val cast_from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes object info to base info *)
val cast_to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Return a Object_info.t from a Base_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Return a Base_info.t from a Object_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Just cast OCaml Ctypes registeredtype info to object info. *)
val cast_from_registeredtypeinfo:
  Registered_type_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes object info to registeredtype info *)
val cast_to_registeredtypeinfo:
  t structure ptr -> Registered_type_info.t structure ptr

(** Return a Object_info.t from a Registered_type_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Registered_type_info.registeredtypeinfo_unref. *)
val from_registeredtypeinfo:
  Registered_type_info.t structure ptr -> t structure ptr

(** Return a Registered_type_info.t form a Object_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Registered_type_info.registeredtypeinfo_unref. *)
val to_registeredtypeinfo:
  t structure ptr -> Registered_type_info.t structure ptr
