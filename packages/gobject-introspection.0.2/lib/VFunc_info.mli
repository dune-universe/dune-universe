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

(** VFunc_info — Struct representing a virtual function. *)

open Ctypes

(** GIVfuncInfo represents a virtual function. A property belongs to either a
    Object_info or a Interface_info. *)
type t
val vfuncinfo : t structure typ

(** Obtain the offset of the function pointer in the class struct. The value
    0xFFFF indicates that the struct offset is unknown. *)
val get_offset:
  t structure ptr -> int

(** Obtain the signal for the virtual function if one is set. The signal comes
    from the object or interface to which this virtual function belongs.
    In order to avoid circular call graph between VFunc_info and Signal_info,
    this function will return a Callable_info. It is upto the user to use
    Signal_info.from_callableinfo in order to have the Signal_info.*)
val get_signal:
  t structure ptr -> Callable_info.t structure ptr option

(** Obtain the flags for this virtual function info. See VFunc_infoFlags for
    more information about possible flag values. *)
val get_flags:
  t structure ptr -> Bindings.VFunc_info.flags list

(*
  TODO : Function_info *	g_vfunc_info_get_invoker ()
  TODO : gpointer	g_vfunc_info_get_address ()
  TODO : gboolean	g_vfunc_info_invoke ()
 *)

(** Just cast OCaml Ctypes base info to vfunc info. *)
val cast_from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes vfunc info to base info *)
val cast_to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Add unref of the C underlying structure whith Gc.finalise. *)
val add_unref_finaliser:
  t structure ptr -> t structure ptr

(** Return a VFunc_info.t from a Base_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Return a Base_info.t from a VFunc_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Just cast OCaml Ctypes callable info to vfunc info. *)
val cast_from_callableinfo:
  Callable_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes vfunc info to callable info *)
val cast_to_callableinfo:
  t structure ptr -> Callable_info.t structure ptr

(** Return a VFunc_info.t from a Callable_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val from_callableinfo:
  Callable_info.t structure ptr -> t structure ptr

(** Return a Callable_info.t from a VFunc_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val to_callableinfo:
  t structure ptr -> Callable_info.t structure ptr
