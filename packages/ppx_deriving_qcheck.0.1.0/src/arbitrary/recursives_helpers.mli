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

type ty = string

(** [is_recursiverow_field ty rw] traverses [rw] looking for [ty] *)
val is_recursive_row_field : loc:location -> ty -> row_field -> bool

(** [is_recursiverow_fields ty rws] traverses [rws] looking for [ty] *)
val is_recursive_row_fields : loc:location -> ty -> row_field list -> bool

(** [is_recursive_constructor_declaration ty constr_decl] traverses [constr_decl]
    looking for [ty] *)
val is_recursive_constructor_declaration :
  loc:location -> ty -> constructor_declaration -> bool

(** [is_recursive_type_declaration td] does a tree traversal of [td]
    and returns true if the type inside [td] is self recursive. *)
val is_recursive_type_declaration : ?loc:location -> type_declaration -> bool

(** [get_recursives_types_declarations tds] returns the sub-list of
    type_declaration inside [tds] where {!is_recursive_type_declaration} returns
    true *)
val get_recursives_type_declarations :
  ?loc:location -> type_declaration list -> ty list
