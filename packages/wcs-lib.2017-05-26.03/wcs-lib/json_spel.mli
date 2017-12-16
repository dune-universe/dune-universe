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

(** Json with embedded expressions utilities. *)

open Wcs_t
open Json_spel_t


(** {6 Builders} *)

val null : json_spel
(** The [null] value of JSON. *)

val int : int -> json_spel
(** [int n] build the value of JSON [n]. *)

val bool : bool -> json_spel
(** [bool b] build the value of JSON [b]. *)

val string : string -> json_spel
(** [string s] build the value of JSON [s]. *)

val assoc : (string * json_spel) list -> json_spel
(** [assoc o] build the JSON object [o]. *)

val list : json_spel list -> json_spel
(** [list l] build the JSON list [l]. *)


(** {6 [json]/[json_spel] conversion} *)

val to_json : json_spel -> json
(** [to_json v] convert [v] value into [json]. Expressions are turned
    into strings with using the Spel concrete syntax. *)

val of_json : json -> json_spel
(** [of_json j] convert [j] into [json_spel]. Strings literals in
    [j] are parsed as text containing quoted spel expressions. *)


(** {6 Manipulation functions} *)

val set : json_spel -> string -> json_spel -> json_spel
(**
   [set o x v] add (or replace) the a field [x] of the object [o] with
   value [v].
*)

val get : json_spel -> string -> json_spel option
(**
   [get o x] gets the value of the field [x] of the object [o].
*)

val take : json_spel -> string -> json_spel * json_spel option
(**
   [take o x] gets the value of the field [x] of the object [o] and
   remove the field from the object. The left part of the return value
   is the modified object and the right part is the value of the
   field.
*)

val assign : json_spel list -> json_spel
(**
   [assign [o1; ...; on]] create a json object that contains all the
   fields of the objets [o1], ..., [on]. It is similare the the JavaScript
   function [Object.assing({}, o1, ... on)].
*)

val push : json_spel -> string -> json_spel -> json_spel
(**
   [push o x v] add the value [v] in the list stored in a field [x]
   of the object [o]. It the field [x] doesn't exists, it creates it.
*)

val pop : json_spel -> string -> json_spel * json_spel option
(**
   [pop o x] take a value in a list stored in the field [x] of [o].
*)


(** {6 Settes and getters} *)

(** {8 Boolean fields} *)

val set_bool : json_spel -> string -> bool -> json_spel
(**
   [set_bool o x b] sets the a field [x] of the object [o] with value
   [b].
*)

val get_bool : json_spel -> string -> bool option
(**
   [get_bool o x] gets the value of the field [x] of the object [o].
*)

(** {8 String fields} *)

val set_string : json_spel -> string -> string -> json_spel
(**
   [set_string o x x] sets the a field [x] of the object [o] with string
   [s].
*)

(* val get_string : json_spel -> string -> string option *)
(* (\** *)
(*    [get_string o x] gets the value of the field [x] of the object [o]. *)
(* *\) *)

(* val take_string : json_spel -> string -> json_spel * string option *)
(* (\** *)
(*    [take_string o x] takes the value of the field [x] of the object [o]. *)
(* *\) *)
