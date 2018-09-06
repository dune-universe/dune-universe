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

(** {6. Locations} *)

type location = Lexing.position * Lexing.position

(** BMD AST *)

type bmd_schema = {
  schema_desc : bmd_schema_desc;
  schema_loc : location;
}
and bmd_schema_desc = bmd_concept list

and bmd_concept = {
  concept_desc : bmd_concept_desc;
  concept_loc : location;
}
and bmd_concept_desc = bmd_concept_name * bmd_concept_name option * bmd_type (* First name is current concept ; Second name is the concept it derives from *)

and bmd_type = {
  type_desc : bmd_type_desc;
  type_loc : location;
}

and bmd_type_desc =
  | BT_string
  | BT_int
  | BT_real
  | BT_boolean
  | BT_date
  | BT_duration
  | BT_rec of (bmd_field_name * bmd_type) list
  | BT_enum of string list
  | BT_ref of bmd_concept_name

and bmd_concept_name = string
and bmd_field_name = string

(** BMD Construction *)

type bmd_constr =
  | BC_concept of (bmd_concept_name * bmd_concept_name option)
  | BC_enum of (bmd_concept_name * string list)
  | BC_field of (bmd_concept_name * bmd_field_name * bmd_type)

type bmd_spec = bmd_constr list

