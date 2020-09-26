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

(** Common to .shp, .shx, etc. *)

type header = {
    length: int;     (** file length in 16-bits words *)
    version: int;    (** version number *)
    shape_type: int; (** type of shapes found in the file *)
    bbox: D3M.bbox   (** bounding box (actual extent of shapes in the file) *)
  } (** .shp and .shx file header *)

val print_header: header -> unit
(**/**)
val header: Bitstring.t -> header * Bitstring.t

val b2i: Int32.t -> int
val get_int: Bitstring.t -> int * Bitstring.t

module ShpD2 : sig
  val make_point: Bitstring.t -> D2.point * Bitstring.t
  val multipoint: Bitstring.t -> D2.bbox * D2.point array * Bitstring.t
  val multishape: Bitstring.t -> D2.bbox * D2.point array array * Bitstring.t
end

module ShpD2M : sig
  val make_point: Bitstring.t -> D2M.point * Bitstring.t
  val multipoint: Bitstring.t -> D2M.bbox * D2M.point array * Bitstring.t
  val multishape: Bitstring.t -> D2M.bbox * D2M.point array array * Bitstring.t
end

module ShpD3M : sig
  val make_point: Bitstring.t -> D3M.point * Bitstring.t
  val multipoint: Bitstring.t -> D3M.bbox * D3M.point array * Bitstring.t
  val multishape: Bitstring.t -> D3M.bbox * D3M.point array array * Bitstring.t
end
