(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open PPrint

(* See Monolith.mli for documentation about the following types and values. *)

type appearance

val constant: string -> appearance

val document: document -> appearance

val infix: string -> appearance

type 'a code =
  'a * appearance

(** [custom f] describes a term whose application to a list of actual
    arguments [actuals] is printed as dictated by the document [f actuals].
    This allows performing certain simplifications on the fly; e.g., an
    identity function might decide to print the application [id e] as just
    [e]. Be careful, though. *)
val custom: (document list -> document) -> appearance

(* Accessors. *)

(** [value] is the first projection. *)
val value: 'a code -> 'a

(** [print code actuals] constructs a document which represents an application
    of the value [code] to the actual arguments [actuals]. *)
val print: 'a code -> document list -> document

(** [apply appearance actuals] is short for [print (_, appearance) actuals]. *)
val apply: appearance -> document list -> document

(** [string code] is a string which represents the value [code], applied to
    zero arguments. *)
val string: 'a code -> string
