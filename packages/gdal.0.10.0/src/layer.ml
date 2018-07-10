open Ctypes

type t = T.t
let t = T.t
let t_opt = T.t_opt

exception Layer_error

let err = T.err Layer_error

(* Raw bindings *)
let get_name =
  Lib.c "OGR_L_GetName"
    (t @-> returning string)

let get_geom_type =
  Lib.c "OGR_L_GetGeomType"
    (t @-> returning nativeint)

let get_geom_type t =
  get_geom_type t
  |> Geometry.wkb_of_nativeint

let get_spatial_filter =
  Lib.c "OGR_L_GetSpatialFilter"
    (t @-> returning Geometry.t)

let set_spatial_filter =
  Lib.c "OGR_L_SetSpatialFilter"
    (t @-> Geometry.t @-> returning void)

let set_spatial_filter_rect =
  Lib.c "OGR_L_SetSpatialFilterRect"
    (t @-> double @-> double @-> double @-> double @-> returning void)

let set_attribute_filter =
  Lib.c "OGR_L_SetAttributeFilter"
    (t @-> string @-> returning err)

let reset_reading =
  Lib.c "OGR_L_ResetReading"
    (t @-> returning void)

let get_next_feature =
  Lib.c "OGR_L_GetNextFeature"
    (t @-> returning Feature.t_opt)

let set_next_by_index =
  Lib.c "OGR_L_SetNextByIndex"
    (t @-> int @-> returning err)

let get_feature =
  Lib.c "OGR_L_GetFeature"
    (t @-> int @-> returning Feature.t_opt)

let set_feature =
  Lib.c "OGR_L_SetFeature"
    (t @-> Feature.t @-> returning err)

let create_feature =
  Lib.c "OGR_L_CreateFeature"
    (t @-> Feature.t @-> returning err)

let delete_feature =
  Lib.c "OGR_L_DeleteFeature"
    (t @-> int @-> returning err)

let get_layer_defn =
  Lib.c "OGR_L_GetLayerDefn"
    (t @-> returning Feature.Defn.t)

let get_spatial_ref =
  Lib.c "OGR_L_GetSpatialRef"
    (t @-> returning Spatial_reference.t_opt)

let get_feature_count =
  Lib.c "OGR_L_GetFeatureCount"
    (t @-> int @-> returning int)

let get_extent =
  Lib.c "OGR_L_GetExtent"
    (t @-> ptr T.envelope @-> int @-> returning err)

let get_extent t i =
  let envelope = make T.envelope in
  get_extent t (addr envelope) i;
  let min_x = getf envelope T.envelope_minx in
  let max_x = getf envelope T.envelope_maxx in
  let min_y = getf envelope T.envelope_miny in
  let max_y = getf envelope T.envelope_maxy in
  Geometry.({ min_x; max_x; min_y; max_y })

let test_capability =
  Lib.c "OGR_L_TestCapability"
    (t @-> string @-> returning int)

let test_capability t s =
  test_capability t s <> 0

let create_field =
  Lib.c "OGR_L_CreateField"
    (t @-> Field.Defn.t @-> int @-> returning err)

let delete_field =
  Lib.c "OGR_L_DeleteField"
    (t @-> int @-> returning err)

let reorder_fields =
  Lib.c "OGR_L_ReorderFields"
    (t @-> ptr int @-> returning err)

let reorder_fields t indices =
  let pointer =
    CArray.of_list int indices
    |> CArray.start
  in
  reorder_fields t pointer

let reorder_field =
  Lib.c "OGR_L_ReorderField"
    (t @-> int @-> int @-> returning err)

let alter_field_defn =
  Lib.c "OGR_L_AlterFieldDefn"
    (t @-> int @-> Field.Defn.t @-> int @-> returning err)

let start_transaction =
  Lib.c "OGR_L_StartTransaction"
    (t @-> returning err)

let commit_transaction =
  Lib.c "OGR_L_CommitTransaction"
    (t @-> returning err)

let rollback_transaction =
  Lib.c "OGR_L_RollbackTransaction"
    (t @-> returning err)

let reference =
  Lib.c "OGR_L_Reference"
    (t @-> returning int)

let dereference =
  Lib.c "OGR_L_Dereference"
    (t @-> returning int)

let get_ref_count =
  Lib.c "OGR_L_GetRefCount"
    (t @-> returning int)

let sync_to_disk =
  Lib.c "OGR_L_SyncToDisk"
    (t @-> returning err)

let get_fid_column =
  Lib.c "OGR_L_GetFIDColumn"
    (t @-> returning string)

let get_geometry_column =
  Lib.c "OGR_L_GetGeometryColumn"
    (t @-> returning string)

let set_ignored_fields =
  Lib.c "OGR_L_SetIgnoredFields"
    (t @-> ptr_opt string_opt @-> returning err)

let set_ignored_fields t fields =
  let fields =
    match fields with
    | [] -> None
    | fields ->
      (* Add None at the end to signal the end of the array to OGR *)
      let fields = List.rev (None :: List.rev fields) in
      Some (CArray.of_list string_opt fields |> CArray.start)
  in
  set_ignored_fields t fields

(* Higher level wrappers *)

let map_features layer f =
  let rec loop accu =
    match get_next_feature layer with
    | None -> List.rev accu
    | Some feature ->
      let res = f feature in
      Feature.destroy feature;
      loop (res :: accu)
  in
  loop []

let iter_features layer f =
  let rec loop () =
    match get_next_feature layer with
    | None -> ()
    | Some feature ->
      f feature;
      Feature.destroy feature;
      loop ()
  in
  loop ()
