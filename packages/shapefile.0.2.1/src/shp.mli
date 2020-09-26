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

(** Read and write ESRI Shapefiles. *)

(** This implementation follows the technical description provided by ESRI.
    This description can be found
    {{:https://www.esri.com/library/whitepapers/pdfs/shapefile.pdf}here}. *)

type shape =
  | Null (** the null shape *)
  | Point of D2.point
  | PointM of D2M.point
  | PointZ of D3M.point (** a single point *)
  | MultiPoint of D2.bbox * D2.point array
  | MultiPointM of D2M.bbox * D2M.point array
  | MultiPointZ of D3M.bbox * D3M.point array
	(** a set of points with its bounding box *)
  | PolyLine of D2.bbox * D2.point array array
  | PolyLineM of D2M.bbox * D2M.point array array
  | PolyLineZ of D3M.bbox * D3M.point array array
	(** a set of lines with its bounding box *)
  | Polygon of D2.bbox * D2.point array array
  | PolygonM of D2M.bbox * D2M.point array array
  | PolygonZ of D3M.bbox * D3M.point array array
	(** a polygon (set of rings) with its bounding box *)
  | MultiPatch (** not implemented yet *)
(** The type of a shape. *)

val read: string -> Common.header * shape list
(** [Shp.read file] parses the [file] and returns a list of shapes. *)
