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

(** Signal_info — Struct representing a signal. *)

open Ctypes

(** Signal_info represents a signal. It's a sub-struct of Callable_info and
    contains a set of flags and a class closure.
    See Callable_info for information on how to retreive arguments and other
    metadata from the signal. *)
type t
val signalinfo : t structure typ

(** Obtain if the returning true in the signal handler will stop the emission
    of the signal. *)
val true_stops_emit:
  t structure ptr -> bool

(** Obtain the flags for this signal info. See GSignalFlags for more
    information about possible flag values. *)
val get_flags:
  t structure ptr -> Bindings.GSignal.flags list

(** Obtain the class closure for this signal if one is set. The class closure
    is a virtual function on the type that the signal belongs to. If the signal
    lacks a closure None will be returned.
    In order to avoid circular call graph between Signal_info and VFunc_info,
    this function will return a Callable_info. It is upto the user to use
    GIVSignalInfo.from_callableinfo in order to have the VFunc_info.*)
val get_class_closure:
  t structure ptr -> Callable_info.t structure ptr option

(** Just cast OCaml Ctypes base info to signal info. *)
val cast_from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes signal info to base info *)
val cast_to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Add unref of the C underlying structure whith Gc.finalise. *)
val add_unref_finaliser:
  t structure ptr -> t structure ptr

(** Return a Signal_info.t from a Base_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val from_baseinfo:
  Base_info.t structure ptr -> t structure ptr

(** Return a Base_info.t from a Signal_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val to_baseinfo:
  t structure ptr -> Base_info.t structure ptr

(** Just cast OCaml Ctypes callable info to signal info. *)
val cast_from_callableinfo:
  Callable_info.t structure ptr -> t structure ptr

(** Just cast OCaml Ctypes signal info to callable info *)
val cast_to_callableinfo:
  t structure ptr -> Callable_info.t structure ptr

(** Return a Signal_info.t from a Callable_info.t, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val from_callableinfo:
  Callable_info.t structure ptr -> t structure ptr

(** Return a Callable_info.t from a Signal_info, the underlying C structure
    ref count is increased and the value is Gc.finalis"ed" with
    Base_info.baseinfo_unref. *)
val to_callableinfo:
  t structure ptr -> Callable_info.t structure ptr

