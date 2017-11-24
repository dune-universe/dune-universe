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

let mk_bmd_concept_with_loc desc loc = {
  concept_desc = desc;
  concept_loc = loc;
}

let mk_bmd_concept cname1 cname2 ctype =
  mk_bmd_concept_with_loc (cname1,cname2,ctype) (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())

let mk_bmd_type_with_loc desc loc = {
  type_desc = desc;
  type_loc = loc;
}

let mk_bmd_type desc =
  mk_bmd_type_with_loc desc (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())

let mk_bmd_string () = mk_bmd_type BT_string
let mk_bmd_int () = mk_bmd_type BT_int
let mk_bmd_real () = mk_bmd_type BT_real
let mk_bmd_boolean () = mk_bmd_type BT_boolean
let mk_bmd_date () = mk_bmd_type BT_date
let mk_bmd_duration () = mk_bmd_type BT_duration
let mk_bmd_rec fields = mk_bmd_type (BT_rec fields)
let mk_bmd_enum values = mk_bmd_type (BT_enum values)
let mk_bmd_ref cname = mk_bmd_type (BT_ref cname)


let mk_bmd_schema_with_loc desc loc = {
  schema_desc = desc;
  schema_loc = loc
}

let mk_bmd_schema desc =
  mk_bmd_schema_with_loc desc (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())

