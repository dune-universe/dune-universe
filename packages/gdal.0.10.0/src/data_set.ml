open Ctypes

type t = T.t
let t = T.t

let t_opt = T.t_opt

exception Invalid_source
exception Invalid_projection
exception Band_error
exception Copy_error
exception Overview_error
exception Wrong_data_type

let proj_err = T.err Invalid_projection
let band_err = T.err Band_error
let copy_err = T.err Copy_error
let overview_err = T.err Overview_error

let open_ = (* 'open' is a keyword in OCaml *)
  Lib.c "GDALOpen"
    (string @-> int @-> returning t)

let close =
  Lib.c "GDALClose"
    (t @-> returning void)

let of_source ?(write = false) name =
  let h = open_ name (if write then 1 else 0) in
  if h = null then
    `Error `Invalid_source
  else
    `Ok h

let of_source_exn ?write name =
  match of_source ?write name with
  | `Ok o -> o
  | `Error _ -> raise Invalid_source

let with_source ?write name f =
  match of_source ?write name with
  | `Ok h -> Lib.protect f h ~finally:close
  | `Error _ as e -> e

let with_source_exn ?write name f =
  let h = of_source_exn ?write name in
  Lib.protect f h ~finally:close

let get_driver =
  Lib.c "GDALGetDatasetDriver"
    (t @-> returning Driver.t)

let get_projection =
  Lib.c "GDALGetProjectionRef"
    (t @-> returning string)

let get_x_size =
  Lib.c "GDALGetRasterXSize"
    (t @-> returning int)

let get_y_size =
  Lib.c "GDALGetRasterYSize"
    (t @-> returning int)

let get_count =
  Lib.c "GDALGetRasterCount"
    (t @-> returning int)

let get_band =
  Lib.c "GDALGetRasterBand"
    (t @-> int @-> returning Band.t)

let get_band_data_type t i =
  let c = get_band t i in
  Band.get_data_type c

let get_band t i kind =
  let c = get_band t i in
  if Band.check_data_type c kind then
    (c, Band.Data.to_ba_kind kind)
  else
    raise Wrong_data_type

let add_band =
  Lib.c "GDALAddBand"
    (t @-> int @-> ptr string_opt @-> returning band_err)

let add_band ?(options = []) t kind =
  let i = Band.Data.to_int kind in
  let options = Lib.convert_creation_options options in
  add_band t i (Lib.creation_options_to_ptr options)

let create_copy =
  Lib.c "GDALCreateCopy" (
    Driver.t @->
    string @->
    t @->
    int @->
    ptr string_opt @-> ptr void @-> ptr void @->
    returning t
  )

let create_copy ?(strict = false) ?(options = []) src driver name =
  let options = Lib.convert_creation_options options in
  let dst =
    create_copy driver name src
      (if strict then 1 else 0)
      (Lib.creation_options_to_ptr options) null null
  in
  if dst = null then
    `Error `Invalid_source
  else
    `Ok dst

let create_copy_exn ?strict ?options src driver name =
  match create_copy ?strict ?options src driver name with
  | `Ok c -> c
  | `Error _ -> raise Invalid_source

let create =
  Lib.c "GDALCreate" (
    Driver.t @-> string @-> int @-> int @-> int @-> int @-> ptr string_opt @->
    returning t
  )

let create ?(options = []) ?bands driver name (columns, rows) =
  let nbands, kind =
    match bands with
    | None -> 0, None
    | Some (n, kind) -> n, Some kind
  in
  let options = Lib.convert_creation_options options in
  let ds =
    create
      driver name columns rows nbands (Band.Data.to_int_opt kind)
      (Lib.creation_options_to_ptr options)
  in
  if ds = null then
    `Error `Invalid_source
  else
    `Ok ds

let create_exn ?options ?bands driver name dims =
  match create ?options ?bands driver name dims with
  | `Ok c -> c
  | `Error _ -> raise Invalid_source

let copy =
  Lib.c "GDALDatasetCopyWholeRaster"
    (t @-> t @-> ptr string_opt @-> ptr void @-> ptr void @-> returning copy_err)

let copy ?(options = []) ~src ~dst =
  let options = Lib.convert_creation_options options in
  copy src dst (Lib.creation_options_to_ptr options) null null

let set_projection =
  Lib.c "GDALSetProjection"
    (t @-> string @-> returning proj_err)

let of_band =
  Lib.c "GDALGetBandDataset"
    (Band.t @-> returning t)

let of_band (band, _) =
  of_band band

let build_overviews =
  Lib.c "GDALBuildOverviews"
    (t @-> string @-> int @-> ptr int @-> int @-> ptr int @-> ptr void @-> ptr void
     @-> returning overview_err)

let build_overviews ?bands t factors resampling =
  let n_factors, factors =
    match factors with
    | [] -> 0, null |> from_voidp int
    | f ->
      let ca = CArray.of_list int f in
      CArray.length ca, CArray.start ca
  in
  let n_bands, bands =
    match bands with
    | None
    | Some [] -> 0, null |> from_voidp int
    | Some b ->
      let ca = CArray.of_list int b in
      CArray.length ca, CArray.start ca
  in
  build_overviews t resampling n_factors factors n_bands bands null null
