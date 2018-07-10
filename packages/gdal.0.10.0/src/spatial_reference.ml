open Ctypes

exception Spatial_reference_error

type t = T.t
let t = T.t
let t_opt = T.t_opt
let err = T.err Spatial_reference_error

let new_spatial_reference =
  Lib.c "OSRNewSpatialReference"
    (ptr_opt string @-> returning t)

let import_from_proj4 =
  Lib.c "OSRImportFromProj4"
    (t @-> string @-> returning err)

let import_from_wkt =
  Lib.c "OSRImportFromWkt"
    (t @-> ptr string @-> returning err)

let set_well_known_geog_cs =
  Lib.c "OSRSetWellKnownGeogCS"
    (t @-> string @-> returning err)

let destroy_spatial_reference =
  Lib.c "OSRDestroySpatialReference"
    (t @-> returning void)

let export_to_proj4 =
  Lib.c "OSRExportToProj4"
    (t @-> ptr (ptr char) @-> returning err)

let export_to_wkt =
  Lib.c "OSRExportToWkt"
    (t @-> ptr (ptr char) @-> returning err)

let export_to_pretty_wkt =
  Lib.c "OSRExportToPrettyWkt"
    (t @-> ptr (ptr char) @-> int @-> returning err)

let free =
  Lib.c "OGRFree"
    (ptr void @-> returning void)

(* Higher level, wrapping functions *)

let make kind spec =
  let sr = new_spatial_reference None in
  Gc.finalise destroy_spatial_reference sr;
  let () =
    match kind with
    | `proj4 -> import_from_proj4 sr spec
    | `wkt ->
      let spec_ptr = allocate string spec in
      import_from_wkt sr spec_ptr
    | `name -> set_well_known_geog_cs sr spec
  in
  sr

(* Based on dbuenzli's string from char array example *)
let string_of_char_ptr p =
  let b = Buffer.create 256 in
  let continue = ref true in
  let i = ref 0 in
  while !continue do
    let c = !@(p +@ !i) in
    if c = '\x00' then continue := false else Buffer.add_char b c;
    incr i
  done;
  Buffer.contents b

let to_proj4 sr =
  let s = allocate (ptr char) (from_voidp char null) in
  export_to_proj4 sr s;
  let result = string_of_char_ptr !@s in
  free (to_voidp !@s);
  result

let to_wkt ?(pretty = false) ?(simplify = false) sr =
  let s = allocate (ptr char) (from_voidp char null) in
  let f x =
    if pretty then (
      export_to_pretty_wkt sr x (if simplify then 1 else 0)
    )
    else (
      export_to_wkt sr x
    )
  in
  f s;
  let result = string_of_char_ptr !@s in
  free (to_voidp !@s);
  result
