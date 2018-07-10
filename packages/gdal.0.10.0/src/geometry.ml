open Ctypes

exception Geometry_error

type envelope_t = {
  min_x : float;
  max_x : float;
  min_y : float;
  max_y : float;
}

type wkb_t =
  | Unknown
  | Point
  | LineString
  | Polygon
  | MultiPoint
  | MultiLineString
  | MultiPolygon
  | GeometryCollection
  | None
  | LinearRing
  | Point25D
  | LineString25D
  | Polygon25D
  | MultiPoint25D
  | MultiLineString25D
  | MultiPolygon25D
  | GeometryCollection25D

let nativeint_of_wkb = function
  | Unknown -> 0n
  | Point -> 1n
  | LineString -> 2n
  | Polygon -> 3n
  | MultiPoint -> 4n
  | MultiLineString -> 5n
  | MultiPolygon -> 6n
  | GeometryCollection -> 7n
  | None -> 100n
  | LinearRing -> 101n
  | Point25D -> 0x80000001n
  | LineString25D -> 0x80000002n
  | Polygon25D -> 0x80000003n
  | MultiPoint25D -> 0x80000004n
  | MultiLineString25D -> 0x80000005n
  | MultiPolygon25D -> 0x80000006n
  | GeometryCollection25D -> 0x80000007n

let wkb_of_nativeint = function
  | 0n -> Unknown
  | 1n -> Point
  | 2n -> LineString
  | 3n -> Polygon
  | 4n -> MultiPoint
  | 5n -> MultiLineString
  | 6n -> MultiPolygon
  | 7n -> GeometryCollection
  | 100n -> None
  | 101n -> LinearRing
  | 0x80000001n -> Point25D
  | 0x80000002n -> LineString25D
  | 0x80000003n -> Polygon25D
  | 0x80000004n -> MultiPoint25D
  | 0x80000005n -> MultiLineString25D
  | 0x80000006n -> MultiPolygon25D
  | 0x80000007n -> GeometryCollection25D
  | _ -> raise Geometry_error

type t = T.t
let t = T.t

let get_name =
  Lib.c "OGR_G_GetGeometryName"
    (t @-> returning string)

let get_type =
  Lib.c "OGR_G_GetGeometryType"
    (t @-> returning nativeint)

let get_type t =
  get_type t
  |> wkb_of_nativeint

let get_point_count =
  Lib.c "OGR_G_GetPointCount"
    (t @-> returning int)

let get_x =
  Lib.c "OGR_G_GetX"
    (t @-> int @-> returning double)

let get_y =
  Lib.c "OGR_G_GetY"
    (t @-> int @-> returning double)

let get_z =
  Lib.c "OGR_G_GetZ"
    (t @-> int @-> returning double)

let clone =
  Lib.c "OGR_G_Clone"
    (t @-> returning t)
