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

open Wcs_lib
open Wcs_j
open Bmd_t
open Bmd_util

let entity_value_of_bmd c =
  let (en,clean_en) = c in
  let syns =
    [ clean_en; "the " ^ clean_en ]
  in { e_val_value = en;
       e_val_metadata = None;
       e_val_synonyms = syns;
       e_val_created = None;
       e_val_updated = None; }

let process_bmd_fields_from_type btype =
  begin match btype with
  | BT_rec rtype -> List.map fst rtype
  | BT_string
  | BT_int
  | BT_real
  | BT_boolean
  | BT_date
  | BT_duration
  | BT_enum _
  | BT_ref _ -> []
  end

let process_bmd_enums_from_type btype =
  begin match btype with
  | BT_enum encontent -> encontent
  | BT_rec _
  | BT_string
  | BT_int
  | BT_real
  | BT_boolean
  | BT_date
  | BT_duration
  | BT_ref _ -> []
  end

let bmd_type_of_concept c =
  let (_,_,bt) = c.concept_desc in bt
let bmd_name_of_concept c =
  let (bn,_,_) = c.concept_desc in bn

let process_bmd_fields bmd =
  let fields =
    List.concat (List.map (fun x -> process_bmd_fields_from_type (bmd_type_of_concept x).type_desc) bmd)
  in
  process_entities fields

let process_bmd_enums bmd =
  let fields =
    List.concat (List.map (fun x -> process_bmd_enums_from_type (bmd_type_of_concept x).type_desc) bmd)
  in
  process_entities fields

let process_bmd_entities bmd =
  let entities = List.map bmd_name_of_concept bmd in
  process_entities entities

let entity_values_of_bmd bmd =
  List.map entity_value_of_bmd bmd

let entities_of_bmd (bmd:bmd_schema) =
  let entity_of_bmd_entity = {
    e_def_entity = "entity";
    e_def_description = None;
    e_def_source = None;
    e_def_open_list = None;
    e_def_values = entity_values_of_bmd (process_bmd_entities bmd.schema_desc);
    e_def_created = None;
    e_def_metadata = None;
    e_def_updated = None;
    e_def_fuzzy_match = None;
  }
  in
  let field_of_bmd_entity = {
    e_def_entity = "field";
    e_def_description = None;
    e_def_source = None;
    e_def_open_list = None;
    e_def_values = entity_values_of_bmd (process_bmd_fields bmd.schema_desc);
    e_def_created = None;
    e_def_metadata = None;
    e_def_updated = None;
    e_def_fuzzy_match = None;
  }
  in
  let enum_of_bmd_entity = {
    e_def_entity = "enum";
    e_def_description = None;
    e_def_source = None;
    e_def_open_list = None;
    e_def_values = entity_values_of_bmd (process_bmd_enums bmd.schema_desc);
    e_def_created = None;
    e_def_metadata = None;
    e_def_updated = None;
    e_def_fuzzy_match = None;
  }
  in [entity_of_bmd_entity;field_of_bmd_entity;enum_of_bmd_entity]

let patch_workspace_with_bmd ws bmd =
  { ws with
    ws_entities = entities_of_bmd bmd; }

let patch_io_in workspace obmd =
  begin match obmd with
  | None -> workspace (* Leaves workspace unchanged *)
  | Some bmd -> patch_workspace_with_bmd workspace bmd
  end

let patch_wcs_workspace wcs_cred ws_id ws_name bmd =
  let ws =
    Wcs.workspace ws_name
      ~entities:(entities_of_bmd bmd)
      ()
  in
  Wcs_call.update_workspace wcs_cred ws_id ws

let bmd_find_entity entity_def_list entity_name =
  List.find (fun x -> x.e_def_entity = entity_name) entity_def_list

let bmd_getentity_values bmd entity_name =
  try
    List.map (fun (x:Wcs_j.entity_value) -> x.e_val_value) ((bmd_find_entity (entities_of_bmd bmd) entity_name).e_def_values)
  with Not_found -> []
