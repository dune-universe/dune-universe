open Ctypes

module Defn = struct
  type t = T.Defn.t
  let t = T.Defn.t

  let get_field_count =
    Lib.c "OGR_FD_GetFieldCount"
      (t @-> returning int)

  let get_field_defn =
    Lib.c "OGR_FD_GetFieldDefn"
      (t @-> int @-> returning Field.Defn.t)
end

type t = T.t
let t = T.t
let t_opt = T.t_opt

let get_as_integer =
  Lib.c "OGR_F_GetFieldAsInteger"
    (t @-> int @-> returning int)

let get_as_double =
  Lib.c "OGR_F_GetFieldAsDouble"
    (t @-> int @-> returning float)

let get_as_string =
  Lib.c "OGR_F_GetFieldAsString"
    (t @-> int @-> returning string)

let get_geometry_ref =
  Lib.c "OGR_F_GetGeometryRef"
    (t @-> returning Geometry.t)

let get_geometry_copy t =
  let g = get_geometry_ref t in
  Geometry.clone g

let destroy =
  Lib.c "OGR_F_Destroy"
    (t @-> returning void)
