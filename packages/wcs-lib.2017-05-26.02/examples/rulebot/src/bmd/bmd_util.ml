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

let not_on_black_list enl =
  begin match enl with
  | (_,"java"::_) -> false
  | (_,"com"::"ibm"::"ia"::_) -> false
  | _ -> true
  end

let rec last_part enl =
  begin match enl with
  | (en,[]) -> raise (Failure "Empty entity")
  | (en,[x]) -> (en,x)
  | (en,_ :: enl') -> last_part (en,enl')
  end

let split_entity en =
  (en,Str.split (Str.regexp "\\.") en)

let process_entities entities =
  let no_dup_entities = List.sort_uniq compare entities in (* XXX Eliminate duplicates -- WCS doesn't like twice the same entity being declare XXX *)
  let split_entities = List.map split_entity no_dup_entities in
  let kept_entities = List.filter not_on_black_list split_entities in
  List.map last_part kept_entities


