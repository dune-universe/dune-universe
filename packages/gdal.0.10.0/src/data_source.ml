open Ctypes

type t = T.t
let t = T.t

exception Data_source_error
exception Invalid_source

let err = T.err Data_source_error

let open_ = (* 'open' is a keyword in OCaml *)
  Lib.c "OGROpen"
    (string @-> int @-> ptr_opt Driver.t @-> returning t)

let destroy =
  Lib.c "OGR_DS_Destroy"
    (t @-> returning void)

let release =
  Lib.c "OGRReleaseDataSource"
    (t @-> returning err)

let get_layer_by_name =
  Lib.c "OGR_DS_GetLayerByName"
    (t @-> string @-> returning Layer.t)

let get_layer =
  Lib.c "OGR_DS_GetLayer"
    (t @-> int @-> returning Layer.t)

let get_layer_count =
    Lib.c "OGR_DS_GetLayerCount"
    (t @-> returning int)

let of_source ?(write = false) name =
  let h = open_ name (if write then 1 else 0) None in
  if h = null then
    `Invalid_source
  else
    `Ok h

let of_source_exn ?write name =
  match of_source ?write name with
  | `Ok h -> h
  | `Invalid_source -> raise Invalid_source

let with_source ?write name f =
  match of_source ?write name with
  | `Ok h -> `Ok (Lib.protect f h ~finally:destroy)
  | `Invalid_source -> `Invalid_source

let with_source_exn ?write name f =
  match with_source ?write name f with
  | `Ok x -> x
  | `Invalid_source -> raise Invalid_source

type driver_t = T.t
let driver_t = T.t
exception Invalid_driver

let get_driver_by_name =
  Lib.c "OGRGetDriverByName"
    (string @-> returning driver_t)

let get_driver_by_name name =
  let d = get_driver_by_name name in
  if d = null then
    None
  else
    Some d

let get_driver_by_name_exn name =
  match get_driver_by_name name with
  | Some d -> d
  | None -> raise Invalid_driver

let create =
  Lib.c "OGR_Dr_CreateDataSource"
    (driver_t @-> string @-> ptr string_opt @-> returning t)

let create ?(options = []) driver name =
  let options = Lib.convert_creation_options options in
  let d = create driver name (Lib.creation_options_to_ptr options) in
  if d = null then
    None
  else
    Some d

let create_exn ?options driver name =
  match create ?options driver name with
  | Some d -> d
  | None -> raise Invalid_source

let copy =
  Lib.c "OGR_Dr_CopyDataSource"
    (driver_t @-> t @-> string @-> ptr string_opt @-> returning t)

let copy ?(options = []) driver src name =
  let options = Lib.convert_creation_options options in
  let dst = copy driver src name (Lib.creation_options_to_ptr options) in
  if dst = null then
    None
  else
    Some dst

let copy_exn ?options driver src name =
  match copy ?options driver src name with
  | Some d -> d
  | None -> raise Invalid_source

let create_layer =
  Lib.c "OGR_DS_CreateLayer"
    (t @-> string @-> Spatial_reference.t_opt @-> nativeint @->
     ptr string_opt @-> returning Layer.t_opt)

let create_layer
    ?spatial_reference ?(geometry_type = Geometry.Unknown) ?(options = [])
    ds name =
  let options = Lib.convert_creation_options options in
  create_layer ds name spatial_reference
    (Geometry.nativeint_of_wkb geometry_type)
    (Lib.creation_options_to_ptr options)

let create_layer_exn ?spatial_reference ?geometry_type ?options ds name =
  match create_layer ?spatial_reference ?geometry_type ?options ds name with
  | None -> raise Data_source_error
  | Some l -> l

let copy_layer =
  Lib.c "OGR_DS_CopyLayer"
    (t @-> Layer.t @-> string @-> ptr string_opt @-> returning Layer.t_opt)

let copy_layer ?(options = []) ds src name =
  let options = Lib.convert_creation_options options in
  copy_layer ds src name (Lib.creation_options_to_ptr options)

let copy_layer_exn ?options ds src name =
  match copy_layer ?options ds src name with
  | None -> raise Data_source_error
  | Some l -> l

