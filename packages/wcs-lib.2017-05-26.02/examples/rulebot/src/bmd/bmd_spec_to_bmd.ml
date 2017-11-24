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

open Wcs_j
open Bmd_t
open Bmd_builder

let rec find_fields cname spec =
  begin match spec with
  | []
  | BC_concept _ :: _
  | BC_enum _ :: _ -> []
  | BC_field (cname',fname,btype) :: spec ->
      if (cname' = cname)
      then (fname,btype) :: (find_fields cname spec)
      else  (find_fields cname spec)
  end

let bmd_rec_of_fields fields =
  mk_bmd_rec fields

let bmd_concept_of_concept spec cname1 cname2 =
  let fields = find_fields cname1 spec in
  mk_bmd_concept cname1 cname2 (bmd_rec_of_fields fields)

let bmd_concept_of_enum cname enumlist =
  mk_bmd_concept cname None (mk_bmd_enum enumlist)

let rec bmd_concepts_of_spec spec =
  begin match spec with
  | [] ->
      []
  | BC_concept (cname1,cname2) :: spec ->
      (bmd_concept_of_concept spec cname1 cname2) :: (bmd_concepts_of_spec spec)
  | BC_enum (cname,enumlist) :: spec ->
      (bmd_concept_of_enum cname enumlist) :: (bmd_concepts_of_spec spec)
  | BC_field _ :: spec ->
      bmd_concepts_of_spec spec
  end

let bmd_schema_of_spec spec =
  mk_bmd_schema (bmd_concepts_of_spec spec)
