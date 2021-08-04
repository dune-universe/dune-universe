(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
open Ppxlib

(** Module to provide pretty-printer and to_str function for Ppxlib.Ast types *)

(** Type_declaration to string *)
val type_decl_to_str : type_declaration -> string

(** Cstr to string *)
val type_cstr_to_str : core_type * core_type * location -> string

(** Cstrs to string *)
val type_cstrs_to_str : (core_type * core_type * location) list -> string

(** Param to string *)
val type_param_to_str : core_type * (variance * injectivity) -> string

(** Params to string *)
val type_params_to_str : (core_type * (variance * injectivity)) list -> string

(** Variance to string *)
val variance_to_str : variance -> string

(** Injectivity_to_str to string*)
val injectivity_to_str : injectivity -> string

(** Core_type to string *)
val core_type_to_str : core_type -> string

(** Core_types to string *)
val core_types_to_str : core_type list -> string

(** Type kind to string *)
val type_kind_to_str : type_kind -> string

(** Label_declaration to string *)
val label_declaration_to_str : label_declaration -> string

(** Constructor declaration to string *)
val constr_declaration_to_str : constructor_declaration -> string

(** Constructor declarations to string *)
val constr_declarations_to_str : constructor_declaration list -> string

(** Constructor argument *)
val constr_args_to_str : constructor_arguments -> string

(** Attribute to string *)
val attribute_to_str : attribute -> string

(** Attributes to string *)
val attributes_to_str : attributes -> string

(** Manifest to string *)
val manifest_to_str : core_type option -> string

(** Longident to string *)
val longident_to_str : longident -> string

(** Private flag to string *)
val private_to_str : private_flag -> string

(** Print type_declaration *)
val print_type_decl : type_declaration -> unit
