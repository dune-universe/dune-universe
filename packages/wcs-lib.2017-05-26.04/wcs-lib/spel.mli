(*
 *  This file is part of the Watson Conversation Service OCaml API project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)


(** Spel constructors. *)

(** {6 parsers} *)

val of_string : string -> Spel_t.expression
(** [of_string s] parse the Spel expression [s]. *)

val of_text : string -> Spel_t.expression
(** [of_text t] parse the text [t] as a string that can contain
    embedded Spel expressions. *)

(** {6 printers} *)

val to_string : Spel_t.expression -> string
(** [to_string e] pretty-print [e] in the Spel concret syntax. *)

val to_text : Spel_t.expression -> string
(** [to_text e] pretty-print [e] as a string embedding the expression e. *)

(** {6 from OCaml types} *)

val entity :
  Wcs_t.entity_def ->
  ?value:Wcs_t.entity_value ->
  unit ->
  Spel_t.expression

val intent :
  Wcs_t.intent_def ->
  Spel_t.expression

val bool :
  bool ->
  Spel_t.expression

val int :
  int ->
  Spel_t.expression

val string :
  string ->
  Spel_t.expression

(** {6 expression constructors} *)

val prop :
  Spel_t.expression -> string ->
  Spel_t.expression

val prop_catch :
  Spel_t.expression -> string ->
  Spel_t.expression

val get :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val list :
  Spel_t.expression list ->
  Spel_t.expression

val new_array :
  Spel_t.spel_type -> int option list -> Spel_t.expression list option ->
  Spel_t.expression

val new_ :
  string -> Spel_t.expression list ->
  Spel_t.expression

val call :
  Spel_t.expression option -> string -> Spel_t.expression list ->
  Spel_t.expression

val call_catch :
  Spel_t.expression option -> string -> Spel_t.expression list ->
  Spel_t.expression

val op :
  Spel_t.op -> Spel_t.expression list ->
  Spel_t.expression

val eq :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val ne :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val lt :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val gt :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val ge :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val not :
  Spel_t.expression ->
  Spel_t.expression

val and_ :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val or_ :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val plus :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val minus :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val uminus :
  Spel_t.expression ->
  Spel_t.expression

val mult :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val div :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val mod_ :
  Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val concat :
  Spel_t.expression list ->
  Spel_t.expression

val conditional :
  Spel_t.expression -> Spel_t.expression -> Spel_t.expression ->
  Spel_t.expression

val to_string :
  Spel_t.expression ->
  Spel_t.expression

val ident :
  string ->
  Spel_t.expression

(** {6 other constructors} *)

val anything_else :
  Spel_t.expression

val context :
  Spel_t.expression

val conversation_start :
  Spel_t.expression

val entitites :
  Spel_t.expression

val input :
  Spel_t.expression

val intents :
  Spel_t.expression

val output :
  Spel_t.expression

val variable :
  string ->
  Spel_t.expression


(** {6 Spel checker} *)

val spel_check : string -> string
(** [spel_check expr] parses the spel expression and prints it
    back. Fall backs to the initial string with a warning in case of
    error. *)

