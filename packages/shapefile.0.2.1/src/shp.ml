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

let record_header = fun bits ->
  match%bitstring bits with
  | {| n: 32 : bigendian; l: 32 : bigendian; payload: -1 : bitstring |} ->
      b2i n, b2i l, payload
  | {|_|} -> failwith "Shp.record_header"

type shape =
  | Null
  | Point of D2.point | PointM of D2M.point | PointZ of D3M.point
  | MultiPoint of D2.bbox * D2.point array
  | MultiPointM of D2M.bbox * D2M.point array
  | MultiPointZ of D3M.bbox * D3M.point array
  | PolyLine of D2.bbox * D2.point array array
  | PolyLineM of D2M.bbox * D2M.point array array
  | PolyLineZ of D3M.bbox * D3M.point array array
  | Polygon of D2.bbox * D2.point array array
  | PolygonM of D2M.bbox * D2M.point array array
  | PolygonZ of D3M.bbox * D3M.point array array
  | MultiPatch

let record = fun bits ->
  let shape_type, bits = get_int bits in
  match shape_type with
  | 00 -> Null, bits
  | 01 -> let p, r = ShpD2.make_point bits in Point p, r
  | 11 -> let p, r = ShpD3M.make_point bits in PointZ p, r
  | 21 -> let p, r = ShpD2M.make_point bits in PointM p, r
  | 08 -> let box, pts, r = ShpD2.multipoint bits in MultiPoint (box, pts), r
  | 18 -> let box, pts, r = ShpD3M.multipoint bits in MultiPointZ (box, pts), r
  | 28 -> let box, pts, r = ShpD2M.multipoint bits in MultiPointM (box, pts), r
  | 03 -> let box, shps, r = ShpD2.multishape bits in PolyLine (box, shps), r
  | 05 -> let box, shps, r = ShpD2.multishape bits in Polygon (box, shps), r
  | 13 -> let box, shps, r = ShpD3M.multishape bits in PolyLineZ (box, shps), r
  | 15 -> let box, shps, r = ShpD3M.multishape bits in PolygonZ (box, shps), r
  | 23 -> let box, shps, r = ShpD2M.multishape bits in PolyLineM (box, shps), r
  | 25 -> let box, shps, r = ShpD2M.multishape bits in PolygonM (box, shps), r
  | 31 -> failwith "MultiPatch shape not implemented yet"
  |  n -> failwith (string_of_int n ^ " is not a documented shape")

let records = fun bits ->
  let res = ref [] and bits = ref bits in
  while Bitstring.bitstring_length !bits > 0 do
    let _number, _length, payload = record_header !bits in
    let record, rest = record payload in
    res := record :: !res;
    bits := rest
  done; List.rev !res

let read = fun file ->
  let bits = Bitstring.bitstring_of_file file in
  let header, contents = header bits in
  header, records contents
