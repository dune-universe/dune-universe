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

(** Interface_info — Struct representing a GInterface *)

open Ctypes

(** Interface_info represents a GInterface type.
    A GInterface has methods, fields, properties, signals, interfaces,
    constants, virtual functions and prerequisites.*)
type t
val interfaceinfo : t structure typ

(** Obtain the number of prerequisites for this interface type. A prerequisites
    is another interface that needs to be implemented for interface, similar to
    an base class for GObjects. *)
val get_n_prerequisites:
  t structure ptr -> int

(** Obtain an interface type prerequisites index n . *)
val get_prerequisite:
  t structure ptr -> int -> Base_info.t structure ptr

(** Obtain the number of properties that this interface type has.*)
val get_n_properties:
  t structure ptr -> int

(** Obtain an interface type property at index n . *)
val get_property:
  t structure ptr -> int -> Property_info.t structure ptr

(** Obtain the number of methods that this interface type has. *)
val get_n_methods:
  t structure ptr -> int

(** Obtain an interface type method at index n . *)
val get_method:
  t structure ptr -> int -> Function_info.t structure ptr

(** Obtain a method of the interface type given a name . NULL will be returned
    if there's no method available with that name. *)
val find_method:
  t structure ptr -> string -> Function_info.t structure ptr option

(** Obtain the number of signals that this interface type has. *)
val get_n_signals:
  t structure ptr -> int

(** Obtain an interface type signal at index n . *)
val get_signal:
  t structure ptr -> int -> Signal_info.t structure ptr

(** Find a signal of the interface *)
val find_signal:
  t structure ptr -> string -> Signal_info.t structure ptr option

(** Obtain the number of constants that this interface type has. *)
val get_n_constants:
  t structure ptr -> int

(** Obtain an interface type constant at index n . *)
val get_constant:
  t structure ptr -> int -> Constant_info.t structure ptr

(** Returns the layout C structure associated with this GInterface. *)
val get_iface_struct:
  t structure ptr -> Struct_info.t structure ptr option

(** Obtain the number of virtual functions that this interface type has. *)
val get_n_vfuncs:
  t structure ptr -> int

(** Obtain an interface type virtual function at index n . *)
val get_vfunc:
  t structure ptr -> int -> VFunc_info.t structure ptr

(** Locate a virtual function slot with name name . See the documentation for
    g_object_info_find_vfunc() for more information on virtuals. *)
val find_vfunc:
  t structure ptr -> string -> VFunc_info.t structure ptr option

(** Just cast OCaml Ctypes base info to interface info. *)
val cast_from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes interface info to base info *)
val cast_to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Add unref of the C underlying structure whith Gc.finalise. *)
val add_unref_finaliser:
  t structure ptr -> t structure ptr

(** Return a Interface_info.t from a Base_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Return a Base_info.t from a Interface_info, the underlying C structure
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

(** Return a Interface_info.t from a Registered_type_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Registered_type_info.registeredtypeinfo_unref. *)
val from_registeredtypeinfo:
  Registered_type_info.t structure ptr -> t structure ptr

(** Return a Registered_type_info.t form a Interface_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Registered_type_info.registeredtypeinfo_unref. *)
val to_registeredtypeinfo:
  t structure ptr -> Registered_type_info.t structure ptr
