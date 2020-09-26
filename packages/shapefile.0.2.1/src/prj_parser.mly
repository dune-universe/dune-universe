/* Copyright 2018 Cyril Allignol
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
 * under the License. */

%{
(* http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html *)
open Prj_syntax
%}
%token <float>N
%token <string>STRING
%token <Prj_syntax.Axis.direction>DIRECTION
%token LP RP EOF
%token PARAMETER PARAM_MT CONCAT_MT INVERSE_MT PASSTHROUGH_MT
%token UNIT AUTHORITY AXIS TOWGS84 PROJECTION SPHEROID PRIMEM
%token DATUM VERT_DATUM LOCAL_DATUM
%token PROJCS GEOGCS GEOCCS VERT_CS COMPD_CS FITTED_CS LOCAL_CS

%type <Prj_syntax.CS.t>prj
%start prj
%%
prj: cs EOF { $1 }

cs: /* coordinate system */
| geographic_cs { CS.Geographic $1 }
| projected_cs { CS.Projected $1 }
| geocentric_cs { CS.Geocentric $1 }
| vert_cs { CS.Vert $1 }
| compd_cs { $1 }
| fitted_cs { $1 }
| local_cs { CS.Local $1 }

projected_cs:
| PROJCS LP STRING geographic_cs projection params unit auth RP
    { { ProjCS.name = $3; geogcs = $4; projection = $5; params = $6;
	linear_unit = $7; axes = Axis.projected_default; authority = $8 } }
| PROJCS LP STRING geographic_cs projection params unit twin_axes auth RP
    { { ProjCS.name = $3; geogcs = $4; projection = $5; params = $6;
	linear_unit = $7; axes = $8; authority = $9 } }

geographic_cs:
| GEOGCS LP STRING datum prime_meridian unit auth RP
    { { GeogCS.name = $3; datum = $4; prime_meridian = $5;
	angular_unit = $6; axes = Axis.geographic_default; authority = $7 } }
| GEOGCS LP STRING datum prime_meridian unit twin_axes auth RP
    { { GeogCS.name = $3; datum = $4; prime_meridian = $5;
	angular_unit = $6; axes = $7; authority = $8 } }

geocentric_cs:
| GEOCCS LP STRING datum prime_meridian unit auth RP
    { { GeocCS.name = $3; datum = $4; prime_meridian = $5;
	linear_unit = $6; axes = Axis.geocentric_default; authority = $7 } }
| GEOCCS LP STRING datum prime_meridian unit triple_axes auth RP
    { { GeocCS.name = $3; datum = $4; prime_meridian = $5;
	linear_unit = $6; axes = $7; authority = $8 } }

vert_cs:
| VERT_CS LP STRING vert_datum unit auth RP
    { { VertCS.name = $3; datum = $4; linear_unit = $5;
	axis = { Axis.name = "Up"; direction = Axis.Up }; authority = $6 } }
| VERT_CS LP STRING vert_datum unit axis auth RP
    { { VertCS.name = $3; datum = $4; linear_unit = $5;
	axis = $6; authority = $7 } }

compd_cs:
| COMPD_CS LP STRING cs cs auth RP { CS.Compd ($3, $4, $5, $6) }

fitted_cs:
| FITTED_CS LP STRING mt cs RP { CS.Fitted ($3, $4, $5) }

local_cs:
| LOCAL_CS LP STRING local_datum unit axis axes auth RP
    { { LocalCS.name = $3; datum = $4; unit = $5;
	axes = $6 :: $7; authority = $8 } }

projection: PROJECTION LP STRING auth RP
    { { Projection.name = $3; authority = $4 } }

datum: DATUM LP STRING spheroid to_wgs84 auth RP
    { { Datum.name = $3; spheroid = $4; toWGS84 = $5; authority = $6 } }

spheroid: SPHEROID LP STRING N N auth RP
    { { Spheroid.name = $3; a = $4; f = $5; authority = $6 } }

prime_meridian: PRIMEM LP STRING N auth RP
    { { Primem.name = $3; longitude = $4; authority = $5 } }

unit: UNIT LP STRING N auth RP
    { { Unit.name = $3; cf = $4; authority = $5 } }

auth: { None } | authority { Some $1 }
authority: AUTHORITY LP STRING STRING RP { { Authority.name = $3; code = $4 } }

vert_datum:
| VERT_DATUM LP STRING N auth RP
    { { Vert_datum.name = $3; datum_type = $4; authority = $5 } }

local_datum:
| LOCAL_DATUM LP STRING N auth RP
    { { Local_datum.name = $3; datum_type = $4; authority = $5 } }

axis: AXIS LP STRING DIRECTION RP { { Axis.name = $3; direction = $4 } }
twin_axes: axis axis { $1, $2 }
triple_axes: axis axis axis { $1, $2, $3 }
axes:
| { [] }
| axis axes { $1 :: $2 }

to_wgs84:
| { None }
| TOWGS84 LP N N N RP
    { Some { ToWGS84.dx = $3; dy = $4; dz = $5; ex = 0.; ey = 0.; ez = 0.; ppm = 0. } }
| TOWGS84 LP N N N N N N RP
    { Some { ToWGS84.dx = $3; dy = $4; dz = $5; ex = $6; ey = $7; ez = $8; ppm = 0. } }
| TOWGS84 LP N N N N N N N RP
    { Some { ToWGS84.dx = $3; dy = $4; dz = $5; ex = $6; ey = $7; ez = $8; ppm = $9 } }

mt: /* math transform */
| param_mt { $1 }
| concat_mt { $1 }
| inv_mt { $1 }
| passthrough_mt { $1 }

mts:
| { [] }
| mt mts { $1 :: $2 }

param_mt:
| PARAM_MT LP STRING params RP { MT.Param ($3, $4) }

parameter: PARAMETER LP STRING N RP { { Parameter.name = $3; value = $4 } }

params:
| { [] }
| parameter params { $1 :: $2 }

concat_mt: CONCAT_MT LP mt mts RP { MT.Concat ($3 :: $4) }

inv_mt: INVERSE_MT LP mt RP { MT.Inverse $3 }
passthrough_mt: PASSTHROUGH_MT LP N mt RP
    { MT.Passthrough (int_of_float $3, $4) }
