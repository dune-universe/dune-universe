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

(** Module expanding a type declaration with a QCheck.arbitrary *)

open Ppxlib

type ty = string

type env = {
  ty : ty;  (** current type under translation *)
  mutual_types : ty list;  (** list of mutual types for the current type *)
  recursives_types : ty list;
      (** list of recursives types, when mutual_types = []
        the only type that might be recursive is [ty] *)
}

(** [env ty] creates default environment with [ty] inside. *)
val env : ty -> env

(** [from_core_type loc env ct] transforms [ct] into a QCheck.arbitrary

    We distinguish two cases:
    - A specific arbitrary is passed throught attribute:
    {[
    type t =
    | Day of int [@arb QCheck.(map (fun x -> Day x) (0--31))] 
    | Month of int
    | Year of int
    [@@deriving arb]
    ]}
    - The arbitrary is derived from the core_type *)
val from_core_type : loc:location -> env:env -> core_type -> expression

(** [from_arrow loc env (left, right)] transform a function type to
    a QCheck.arbitrary.

    TODO: this should be more documented *)
val from_arrow : loc:location -> env:env -> core_type * core_type -> expression

(** [from_ptyp_variant loc env variant] create a QCheck.arbitrary from the
    body of a Ptyp_variant inside [variant] 

    TODO: comment that function, it's really similar to a constructor declaration
    list. *)
val from_ptyp_variant : loc:location -> env:env -> row_field list -> expression

(** [from_type_kind loc env type_kind] creates a QCheck.arbitrary from
    the type description inside type_kind
        
    - [type kind] is a record, we use {!from_record}
    - [type kind] is a variant, we use {!from_variant}
    - Otherwise we return None because the arbitrary could not be derived. *)
val from_type_kind : loc:location -> env:env -> type_kind -> expression option

(** Transform a record into a record QCheck.arbitrary *)

(** [from_record loc env label_decls] create a QCheck.arbitrary using
    the record's description inside [label_decls]

    For each label_declaration, we create an arbitrary as an expression.
    These arbitraries are passed to {!Types_helper.record}. *)
val from_record :
  loc:location -> env:env -> label_declaration list -> expression

(** [from_tuple loc env cts] translate every core_type inside [cts] and
    build a tuple QCheck.arbitrary using {!Types_helper.tuple} *)
val from_tuple : loc:location -> env:env -> core_type list -> expression

(** [from_variant loc env constrs] create a QCheck.arbitrary for the constructors
    in [constrs].

    We distinguish two cases:
    - The type is self recursive
      {[
      type tree = Leaf | Node of int * tree * tree
      ]}

      The distinction betweens recursive nodes and leaves must be considered
      in order to avoid a infinite loop on a recursive type

    - The type is a list of constructor
      {[
      type color = Green | Blue | Red | Any of int
      ]}

      We just have to chose one of the constructors built using
      {!from_constructor_decl}. *)
val from_variant :
  loc:location -> env:env -> constructor_declaration list -> expression

(** [from_constructor_decl loc env cd] returns the pair (weight option * arbitrary)
    for [cd].

    Weight is the optional frequency of the arbitrary in a [QCheck.frequency] as
    constructor declaration might often be inside a list of declaration *)
val from_constructor_decl :
  loc:location ->
  env:env ->
  constructor_declaration ->
  expression option * expression

(** [from_type_declaration loc env td] translate a type_declaration [td] into
    the according QCheck.arbitrary

    {[
    type t = (* type_declaration *)
    [@@deriving arb]

    (* ==> *)

    let arb : t QCheck.arbitrary = (* t arbitrary *)
    ]}

    [env] can either be passed from {!from_type_declarations} with additionals
    information. Otherwise, [from_type_declaration] will build it's own
    environment. *)
val from_type_declaration :
  loc:location -> ?env:env -> type_declaration -> structure_item

(** [from_type_declarations loc tds] translate every type_declaration in [tds]
    using {!from_type_declaration}.

    List of type_declaration comes from a mutual recursive type:
    {[
    type expr =
    | If of expr * expr * expr
    | Assign of char * expr
    | Value of value

    and value =
    | Int of int
    | Bool of bool [@@deriving arb]
    
    (* ==> *)

    let arb_expr () = (* expr arbitrary *)
    and arb_expr = arb_expr ()
    and arb_value () = (* value arbitrary *)
    and arb_value = arb_value ()
    ]}
*)
val from_type_declarations :
  loc:location -> type_declaration list -> structure_item
