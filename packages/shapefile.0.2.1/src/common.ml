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

let b2i = Int32.to_int

module type Data = sig
  val dim: int
  type point
  type bbox
  val a2p: float array -> point
  val a2b: float array -> bbox
end

let get_int = fun bits -> (* littleendian *)
  match%bitstring bits with
  | {| v: 32 : littleendian, bind (b2i v); rem: -1 : bitstring |} -> v, rem

let get_float = fun bits ->
  match%bitstring bits with
  | {| v: 64 : littleendian, bind (Int64.float_of_bits v); rem: -1 : bitstring
     |} -> v, rem

let get_array = fun get_elt size bits ->
  let bits = ref bits in
  let a = Array.init size (fun _ ->
    let v, rem = get_elt !bits in
    bits := rem; v) in
  a, !bits

let get_int_array = get_array get_int
let get_float_array = get_array get_float

module ShapeMake (D : Data) = struct

  let make_point = fun bits ->
    let p, rem = get_float_array D.dim bits in
    D.a2p p, rem

  let make_bbox = fun bits ->
    let b, rem = get_float_array (2 * D.dim) bits in
    D.a2b b, rem

  let make_parts = fun n bits -> let parts, _ = get_int_array n bits in parts

  let make_points = fun n bits ->
    let pts, _ = get_array make_point n bits in pts

  let make_shapes = fun nparts npoints parts points ->
    let parts = make_parts nparts parts
    and points = make_points npoints points in
    Array.init nparts (fun i ->
      Array.sub points parts.(i)
	((if i < nparts - 1 then parts.(i+1) else npoints) - parts.(i)))

  let multipoint = fun bits ->
    let bbox, bits = make_bbox bits in
    match%bitstring bits with
    | {| npoints: 32 : littleendian, bind (b2i npoints);
       points: D.dim * 64 * npoints : bitstring;
       rest: -1 : bitstring |} ->
       bbox, make_points npoints points, rest

  let multishape = fun bits ->
    let bbox, bits = make_bbox bits in
    match%bitstring bits with
    | {| nparts: 32 : littleendian, bind (b2i nparts);
       npoints: 32 : littleendian, bind (b2i npoints);
       parts: 32 * nparts : bitstring;
       points: D.dim * 64 * npoints : bitstring;
       rest: -1 : bitstring |} ->
       let shapes = make_shapes nparts npoints parts points in
       bbox, shapes, rest

end

module ShpD2 = ShapeMake(D2)
module ShpD2M = ShapeMake(D2M)
module ShpD3M = ShapeMake(D3M)

type header = { length: int; version: int; shape_type: int; bbox: D3M.bbox }

let print_header = fun h ->
  Printf.printf "file length = %d\n%!" h.length;
  Printf.printf "version = %d\n%!" h.version;
  Printf.printf "type = %d\n%!" h.shape_type;
  D3M.print_bbox h.bbox

let header = fun bits ->
  match%bitstring bits with
  | {| code: 32 : bigendian, check (b2i code = 9994);
     _: 5 * 32 : bitstring; (* 5 unused fields (32 bits each) *)
     length: 32 : bigendian, bind (b2i length);
     version: 32 : littleendian, bind (b2i version);
     shape_type: 32 : littleendian, bind (b2i shape_type);
     rest: -1 : bitstring |} ->
     let bbox, contents = ShpD3M.make_bbox rest in
     { length; version; shape_type; bbox }, contents
  | {| _ |} -> failwith "Shp.header"
