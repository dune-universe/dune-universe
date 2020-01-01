(*
 * Copyright 2019 Cedric LE MOIGNE, cedlemo@gmx.com
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

(* Flags defined in the GObject library for the GParamSpec module. I deliberatly
 * choose to embed it in OCaml-GObject-Introspection.
 * Through the GParamFlags flag values, certain aspects of parameters can be
 * configured. *)
type flags =
  | Readable        (** the parameter is readable *)
  | Writable        (** the parameter is writable *)
  | Readwrite       (** alias for G_PARAM_READABLE | G_PARAM_WRITABLE *)
  | Construct       (** the parameter will be set upon object construction *)
  | Construct_only  (** the parameter can only be set upon object construction *)
  | Lax_validation  (** upon parameter conversion (see g_param_value_convert())
                        strict validation is not required *)
  | Static_name     (** the string used as name when constructing the parameter
                        is guaranteed to remain valid and unmodified for the
                        lifetime of the parameter. Since 2.8 *)
  | Static_nick     (** the string used as nick when constructing the parameter
                        is guaranteed to remain valid and unmmodified for the
                        lifetime of the parameter. Since 2.8 *)
  | Static_blurb    (** the string used as blurb when constructing the parameter
                        is guaranteed to remain valid and unmodified for the
                        lifetime of the parameter. Since 2.8 *)
  | Explicit_notify (** calls to g_object_set_property() for this property will
                        not automatically result in a "notify" signal being
                        emitted: the implementation must call g_object_notify()
                        themselves in case the property actually changes.
                        Since: 2.42. *)
  | Deprecated      (** the parameter is deprecated and will be removed in a
                        future version. A warning will be generated if it is
                        used while running with G_ENABLE_DIAGNOSTIC=1.
                        Since 2.26 *)

let string_of_flag = function
  | Readable -> "Readable"
  | Writable -> "Writable"
  | Readwrite -> "Readwrite"
  | Construct -> "Construct"
  | Construct_only -> "Construct_only"
  | Lax_validation -> "Lax_validation"
  | Static_name -> "Static_name"
  | Static_nick -> "Static_nick"
  | Static_blurb -> "Static_blurb"
  | Explicit_notify -> "Explicit_notify"
  | Deprecated -> "Deprecated"

module Flags = functor (T : Cstubs.Types.TYPE) -> struct
  let g_param_readable = T.constant "G_PARAM_READABLE" T.int64_t
  let g_param_writable = T.constant "G_PARAM_WRITABLE" T.int64_t
  let g_param_readwrite = T.constant "G_PARAM_READWRITE" T.int64_t
  let g_param_construct = T.constant "G_PARAM_CONSTRUCT" T.int64_t
  let g_param_construct_only = T.constant "G_PARAM_CONSTRUCT_ONLY" T.int64_t
  let g_param_lax_validation = T.constant "G_PARAM_LAX_VALIDATION" T.int64_t
  let g_param_static_name = T.constant "G_PARAM_STATIC_NAME" T.int64_t
  let g_param_static_nick = T.constant "G_PARAM_STATIC_NICK" T.int64_t
  let g_param_static_blurb = T.constant "G_PARAM_STATIC_BLURB" T.int64_t
  let g_param_explicit_notify = T.constant "G_PARAM_EXPLICIT_NOTIFY" T.int64_t
  let g_param_deprecated = T.constant "G_PARAM_DEPRECATED" T.int64_t
  let flags = T.enum "GParamFlags" ~typedef:true [] ~unexpected:(fun x -> x)
end
