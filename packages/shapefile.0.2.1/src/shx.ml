(* Copyright 2018 Cyril Allignol
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. *)

open Common

type index = { offset: int; length: int }

let record = fun bits ->
  match%bitstring bits with
  | {| offset: 32 : bigendian, bind (b2i offset);
     length: 32 : bigendian, bind (b2i length);
     r: -1 : bitstring |} -> { offset; length }, r

let records = fun bits ->
  let res = ref [] and bits = ref bits in
  while Bitstring.bitstring_length !bits > 0 do
    let index, rest = record !bits in
    res := index :: !res;
    bits := rest
  done; List.rev !res

let read = fun file ->
  let bits = Bitstring.bitstring_of_file file in
  let header, contents = header bits in
  header, records contents
