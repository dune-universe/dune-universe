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

let sprint_opt sprint = function
  | None -> ""
  | Some e -> Printf.sprintf ", %s" (sprint e)

let sprint_list = fun sprint l ->
  List.fold_left (fun acc e -> acc ^ ", " ^ (sprint e)) "" l

module Parameter = struct
  type t = { name: string; value: float }
  let sprint = fun p -> Printf.sprintf "PARAMETER[\"%s\", %f]" p.name p.value
end

module MT = struct
  type t =
    | Param of string * Parameter.t list
    | Concat of t list
    | Inverse of t
    | Passthrough of int * t

  let rec sprint = function
    | Param (name, ps) ->
	Printf.sprintf "PARAM_MT[\"%s\"%s]" name
	  (sprint_list Parameter.sprint ps)
    | Concat (mt :: mts) ->
	Printf.sprintf "CONCAT_MT[%s%s]" (sprint mt) (sprint_list sprint mts)
    | Concat _ -> invalid_arg "Prj_syntax.MT"
    | Inverse mt -> Printf.sprintf "INVERSE_MT[%s]" (sprint mt)
    | Passthrough (i, mt) ->
	Printf.sprintf "PASSTHROUGH_MT[%d, %s]" i (sprint mt)

end

module Authority = struct
  type t = { name: string; code: string }
  let sprint = fun a -> Printf.sprintf "AUTHORITY[\"%s\", \"%s\"]" a.name a.code
end

module Axis = struct
  type direction = North | South | East | West | Up | Down | Other
  type t = { name: string; direction: direction }
  let sprint_dir = function
    | North -> "NORTH" | South -> "SOUTH" | East -> "EAST" | West -> "WEST"
    | Up -> "UP" | Down -> "DOWN" | Other -> "OTHER"
  let sprint = fun a ->
    Printf.sprintf "AXIS[\"%s\", %s]" a.name (sprint_dir a.direction)

  let geographic_default =
    { name = "Lon"; direction = East }, { name = "Lat"; direction = North }
  let projected_default =
    { name = "X"; direction = East }, { name = "Y"; direction = North }
  let geocentric_default =
    { name = "X"; direction = Other },
    { name = "Y"; direction = East }, { name = "Z"; direction = North }
end

module Unit = struct
  type t = { name: string; cf: float; authority: Authority.t option }
  let sprint = fun u ->
    Printf.sprintf "UNIT[\"%s\", %f%s]" u.name u.cf
      (sprint_opt Authority.sprint u.authority)
end

module Primem = struct
  type t = { name: string; longitude: float; authority: Authority.t option }
  let sprint = fun pm ->
    Printf.sprintf "PRIMEM[\"%s\", %f%s]" pm.name pm.longitude
      (sprint_opt Authority.sprint pm.authority)
end

module ToWGS84 = struct
  type t = { dx: float; dy: float; dz: float;
	     ex: float; ey: float; ez: float;
	     ppm: float }
  let sprint = fun t ->
    Printf.sprintf "TOWGS84[%f, %f, %f, %f, %f, %f, %f]"
      t.dx t.dy t.dz t.ex t.ey t.ez t.ppm
end

module Spheroid = struct
  type t = { name: string; a: float; f: float; authority: Authority.t option }
  let sprint = fun s ->
    Printf.sprintf "SPHEROID[\"%s\", %f, %f%s]"
      s.name s.a s.f (sprint_opt Authority.sprint s.authority)
end

module Datum = struct
  type t = { name: string; spheroid: Spheroid.t;
	     toWGS84: ToWGS84.t option; authority: Authority.t option }
  let sprint = fun s ->
    Printf.sprintf "DATUM[\"%s\", %s%s%s]" s.name (Spheroid.sprint s.spheroid)
      (sprint_opt ToWGS84.sprint s.toWGS84)
      (sprint_opt Authority.sprint s.authority)
end

module Vert_datum = struct
  type t = { name: string; datum_type: float; authority: Authority.t option }
  let sprint = fun vd ->
    Printf.sprintf "VERT_DATUM[\"%s\", %f%s]"
      vd.name vd.datum_type (sprint_opt Authority.sprint vd.authority)
end

module Local_datum = struct
  type t = { name: string; datum_type: float; authority: Authority.t option }
  let sprint = fun ld ->
    Printf.sprintf "LOCAL_DATUM[\"%s\", %f%s]"
      ld.name ld.datum_type (sprint_opt Authority.sprint ld.authority)
end

module Projection = struct
  type t = { name: string; authority: Authority.t option }
  let sprint = fun p ->
    Printf.sprintf "PROJECTION[\"%s\"%s]"
      p.name (sprint_opt Authority.sprint p.authority)
end

module GeogCS = struct
  type t = { name: string; datum: Datum.t; prime_meridian: Primem.t;
	     angular_unit: Unit.t; axes: Axis.t * Axis.t;
	     authority: Authority.t option }
  let sprint = fun g ->
    let lon_axis, lat_axis = g.axes in
    Printf.sprintf "GEOGCS[\"%s\", %s, %s, %s, %s, %s%s]"
      g.name (Datum.sprint g.datum) (Primem.sprint g.prime_meridian)
      (Unit.sprint g.angular_unit) (Axis.sprint lon_axis) (Axis.sprint lat_axis)
      (sprint_opt Authority.sprint g.authority)
end

module ProjCS = struct
  type t = { name: string; geogcs: GeogCS.t; projection: Projection.t;
	     params: Parameter.t list; linear_unit: Unit.t;
	     axes: Axis.t * Axis.t; authority: Authority.t option }
  let sprint = fun p ->
    let x_axis, y_axis = p.axes in
    Printf.sprintf "PROJCS[\"%s\", %s, %s, %s, %s, %s, %s%s]"
      p.name (GeogCS.sprint p.geogcs) (Projection.sprint p.projection)
      (sprint_list Parameter.sprint p.params) (Unit.sprint p.linear_unit)
      (Axis.sprint x_axis) (Axis.sprint y_axis)
      (sprint_opt Authority.sprint p.authority)
end

module GeocCS = struct
  type t = { name: string; datum: Datum.t; prime_meridian: Primem.t;
	     linear_unit: Unit.t; axes: Axis.t * Axis.t * Axis.t;
	     authority: Authority.t option }
  let sprint = fun g ->
    let x_axis, y_axis, z_axis = g.axes in
    Printf.sprintf "GEOCCS[\"%s\", %s, %s, %s, %s, %s, %s%s]"
      g.name (Datum.sprint g.datum) (Primem.sprint g.prime_meridian)
      (Unit.sprint g.linear_unit)
      (Axis.sprint x_axis) (Axis.sprint y_axis) (Axis.sprint z_axis)
      (sprint_opt Authority.sprint g.authority)
end

module VertCS = struct
  type t = { name: string; datum: Vert_datum.t; linear_unit: Unit.t;
	     axis: Axis.t; authority: Authority.t option }
  let sprint = fun vd ->
    Printf.sprintf "VERT_CS[\"%s\", %s, %s, %s%s]"
      vd.name (Vert_datum.sprint vd.datum) (Unit.sprint vd.linear_unit)
      (Axis.sprint vd.axis) (sprint_opt Authority.sprint vd.authority)
end

module LocalCS = struct
  type t = { name: string; datum: Local_datum.t; unit: Unit.t;
	     axes: Axis.t list; authority: Authority.t option }
  let sprint = fun ld ->
    Printf.sprintf "LOCAL_CS[\"%s\", %s, %s, %s%s]"
      ld.name (Local_datum.sprint ld.datum) (Unit.sprint ld.unit)
      (sprint_list Axis.sprint ld.axes)
      (sprint_opt Authority.sprint ld.authority)
end

module CS = struct
  type t =
    | Geographic of GeogCS.t
    | Projected of ProjCS.t
    | Geocentric of GeocCS.t
    | Vert of VertCS.t
    | Compd of string * t * t * Authority.t option
    | Fitted of string * MT.t * t
    | Local of LocalCS.t

  let rec sprint = function
    | Geographic cs -> GeogCS.sprint cs
    | Projected cs -> ProjCS.sprint cs
    | Geocentric cs -> GeocCS.sprint cs
    | Vert cs -> VertCS.sprint cs
    | Compd (name, head_cs, tail_cs, authority) ->
	Printf.sprintf "COMPD_CS[\"%s\", %s, %s%s]"
	  name (sprint head_cs) (sprint tail_cs)
	  (sprint_opt Authority.sprint authority)
    | Fitted (name, to_base, base_cs) ->
	Printf.sprintf "FITTED_CS[\"%s\", %s, %s]"
	  name (MT.sprint to_base) (sprint base_cs)
    | Local cs -> LocalCS.sprint cs
end
