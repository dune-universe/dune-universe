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
open Bmd_builder
open Bmd_util
open Io_j

let bmd_concept_of_entity cname =
  mk_bmd_concept
    cname
    None
    (mk_bmd_rec []) (* XXX Empty fields for now XXX *)

let bmd_concept_of_brand bts =
  List.map bmd_concept_of_entity bts

let process_brandTypes bts =
  List.map (fun x -> x.io_brand) bts

let bmd_concepts_of_io io_j =
  List.map bmd_concept_of_entity (process_brandTypes io_j.io_schema.io_brandTypes)

let bmd_of_io io_j =
  mk_bmd_schema (bmd_concepts_of_io io_j)

