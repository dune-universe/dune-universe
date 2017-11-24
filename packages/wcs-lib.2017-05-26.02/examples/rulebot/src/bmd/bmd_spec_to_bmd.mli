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

open Bmd_t

val find_fields :
  bmd_concept_name ->
  bmd_constr list -> (bmd_field_name * bmd_type) list
val bmd_rec_of_fields :
  (bmd_field_name * bmd_type) list -> bmd_type
val bmd_concept_of_concept :
  bmd_constr list -> bmd_concept_name -> bmd_concept_name option -> bmd_concept
val bmd_concept_of_enum :
  bmd_concept_name -> string list -> bmd_concept
val bmd_concepts_of_spec : bmd_constr list -> bmd_concept list
val bmd_schema_of_spec : bmd_constr list -> bmd_schema
