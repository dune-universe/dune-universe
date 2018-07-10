open Ctypes

exception Warp_error

let err = T.err Warp_error

type resample_t =
  | Nearest_neighbor
  | Bilinear
  | Cubic
  | Cubic_spline
  | Lanczos

let int_of_resample = function
  | Nearest_neighbor -> 0
  | Bilinear -> 1
  | Cubic -> 2
  | Cubic_spline -> 3
  | Lanczos -> 4

module Options = struct
  module Raw = struct
    type warp_options_t
    type t = warp_options_t structure ptr

    let t : warp_options_t structure typ = structure "GDALWarpOptions"
    let f n k = field t n k
    let warp_options = f "warp_options" (ptr string_opt)
    let memory_limit = f "memory_limit" double
    let resample_alg = f "resample_alg" int
    let data_type = f "data_type" int
    let src_ds = f "src_ds" Data_set.t
    let dst_ds = f "dst_ds" Data_set.t
    let band_count = f "band_count" int
    let src_bands = f "src_bands" (ptr int)
    let dst_bands = f "dst_bands" (ptr int)
    let _src_alpha_band = f "src_alpha_band" int
    let _dst_alpha_band = f "dst_alpha_band" int
    let src_no_data_real = f "src_no_data_real" (ptr_opt double)
    let src_no_data_imag = f "src_no_data_imag" (ptr_opt double)
    let dst_no_data_real = f "dst_no_data_real" (ptr_opt double)
    let dst_no_data_imag = f "dst_no_data_imag" (ptr_opt double)
    let _progress = f "progress" (ptr void)
    let _progress_arg = f "progress_arg" (ptr void)
    let transformer =
      f "transformer" (Foreign.funptr (Transform.transform_t (ptr void)))
    let transformer_arg = f "transformer_arg" (ptr void)
    let _src_per_band_validity_mask_func =
      f "src_per_band_validity_mask_func" (ptr void)
    let _src_per_band_validity_mask_func_arg =
      f "src_per_band_validity_mask_func_arg" (ptr (ptr void))
    let _src_validity_mask_func =
      f "src_validity_mask_func" (ptr void)
    let _src_validity_mask_func_arg =
      f "src_validity_mask_func_arg" (ptr void)
    let _src_density_mask_func =
      f "src_density_mask_func" (ptr void)
    let _src_density_mask_func_arg =
      f "src_density_mask_func_arg" (ptr void)
    let _dst_density_mask_func =
      f "dst_density_mask_func" (ptr void)
    let _dst_density_mask_func_arg =
      f "dst_density_mask_func_arg" (ptr void)
    let _dst_validity_mask_func =
      f "dst_validity_mask_func" (ptr void)
    let _dst_validity_mask_func_arg =
      f "dst_validity_mask_func_arg" (ptr void)
    let _pre_warp_chunk_processor =
      f "pre_warp_chunk_processor" (ptr void)
    let _pre_warp_chunk_processor_arg =
      f "pre_warp_chunk_processor_arg" (ptr void)
    let _post_warp_chunk_processor =
      f "post_warp_chunk_processor" (ptr void)
    let _post_warp_chunk_processor_arg =
      f "post_warp_chunk_processor_arg" (ptr void)
    let _cutline = f "cutline" (ptr void)
    let _cutline_blend_dist = f "cutline_blend_dist" double
    let () = seal t

    let create =
      Lib.c "GDALCreateWarpOptions"
        (void @-> returning (ptr t))

    let clone =
      Lib.c "GDALCloneWarpOptions"
        (ptr t @-> returning (ptr t))

    let delete =
      Lib.c "free"
        (ptr t @-> returning void)

    let create () =
      let o = create () in
      Gc.finalise delete o;
      o
  end

  exception Band_count_mismatch

  (* A type to keep references to values which may otherwise be GC'd before
     it's safe *)
  type 'a t = {
    o : Raw.t;
    mutable options : string option Ctypes.CArray.t option;
    mutable src_bands :
      (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t option;
    mutable dst_bands :
      (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t option;
    mutable src_no_data_real :
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t option;
    mutable src_no_data_imag :
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t option;
    mutable dst_no_data_real :
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t option;
    mutable dst_no_data_imag :
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t option;
  }

  let set_warp_options o options =
    let options = Lib.convert_creation_options options in
    setf !@(o.o) Raw.warp_options (Lib.creation_options_to_ptr options);
    o.options <- options;
    ()

  let set_memory_limit { o; _ } l =
    setf !@o Raw.memory_limit l;
    ()

  let set_resample_alg { o; _ } a =
    let i = int_of_resample a in
    setf !@o Raw.resample_alg i;
    ()

  let set_working_data_type { o; _ } t =
    let i = Band.Data.to_int t in
    setf !@o Raw.data_type i;
    ()

  let set_src { o; _ } ds =
    setf !@o Raw.src_ds ds;
    ()

  let set_dst { o; _ } ds =
    setf !@o Raw.dst_ds ds;
    ()

  let set_bands o bands =
    let src = List.map fst bands |> Array.of_list in
    let dst = List.map snd bands |> Array.of_list in
    let open Bigarray in
    let src = Array1.of_array int c_layout src in
    let dst = Array1.of_array int c_layout dst in
    let n = Array1.dim src in
    setf !@(o.o) Raw.band_count n;
    setf !@(o.o) Raw.src_bands (bigarray_start array1 src);
    setf !@(o.o) Raw.dst_bands (bigarray_start array1 dst);
    o.src_bands <- Some src;
    o.dst_bands <- Some dst;
    ()

  let ptr_opt_of_list = function
    | [] -> None
    | l ->
      let a =
        Array.of_list l
        |> Bigarray.(Array1.of_array float64 c_layout)
      in
      Some a

  let set_band_dep o l f =
    let bands = getf !@(o.o) Raw.band_count in
    let n = List.length l in
    if n > 0 && n <> bands then raise Band_count_mismatch;
    match ptr_opt_of_list l with
    | None ->
      setf !@(o.o) f None;
      None
    | Some ca ->
      let p = bigarray_start array1 ca in
      setf !@(o.o) f (Some p);
      Some ca

  let set_src_no_data_real o l =
    let ca = set_band_dep o l Raw.src_no_data_real in
    o.src_no_data_real <- ca

  let set_src_no_data_imag o l =
    let ca = set_band_dep o l Raw.src_no_data_imag in
    o.src_no_data_imag <- ca

  let set_dst_no_data_real o l =
    let ca = set_band_dep o l Raw.dst_no_data_real in
    o.dst_no_data_real <- ca

  let set_dst_no_data_imag o l =
    let ca = set_band_dep o l Raw.dst_no_data_imag in
    o.dst_no_data_imag <- ca

  let set_transformer { o; _ } transform =
    let c = Transform.get_transform_c transform in
    let arg = Transform.get_transform_t transform in
    setf !@o Raw.transformer c;
    setf !@o Raw.transformer_arg arg;
    ()

  let may f o =
    match o with
    | None -> ()
    | Some x -> f x

  let create () =
    let o = Raw.create () in
    {
      o;
      options = None;
      src_bands = None;
      dst_bands = None;
      src_no_data_real = None;
      src_no_data_imag = None;
      dst_no_data_real = None;
      dst_no_data_imag = None;
    }

  let make ?warp_options ?memory_limit ?resample_alg ?working_data_type
      ?src ?dst ?bands
      ?src_no_data_real ?src_no_data_imag
      ?dst_no_data_real ?dst_no_data_imag
      ?transformer
      () =
    let o = create () in
    let mayo f x = may (f o) x in
    mayo set_warp_options warp_options;
    mayo set_memory_limit memory_limit;
    mayo set_resample_alg resample_alg;
    mayo set_working_data_type working_data_type;
    mayo set_src src;
    mayo set_dst dst;
    mayo set_bands bands;
    mayo set_src_no_data_real src_no_data_real;
    mayo set_src_no_data_imag src_no_data_imag;
    mayo set_dst_no_data_real dst_no_data_real;
    mayo set_dst_no_data_imag dst_no_data_imag;
    mayo set_transformer transformer;
    o

  let clone o =
    { o with o = Raw.clone o.o }
end

let reproject_image =
  Lib.c "GDALReprojectImage"
    (Data_set.t @-> string_opt @-> Data_set.t @-> string_opt @->
     int @-> double @-> double @->
     ptr void @-> ptr void @->
     ptr Options.Raw.t @-> returning err)

let reproject_image ?(memory_limit = 0.0) ?(max_error = 0.0)
    ?options ?src_wkt ?dst_wkt ~src ~dst alg =
  let options =
    match options with
    | None -> from_voidp Options.Raw.t null
    | Some { Options.o; _ } -> o
  in
  reproject_image src src_wkt dst dst_wkt (int_of_resample alg)
    memory_limit max_error null null options

let create_and_reproject_image =
  Lib.c "GDALCreateAndReprojectImage"
    (Data_set.t @-> string_opt @-> string @-> string_opt @-> Driver.t @->
     ptr string_opt @-> int @-> double @-> double @->
     ptr void @-> ptr void @-> ptr Options.Raw.t @-> returning err)

let create_and_reproject_image
    ?(memory_limit = 0.0) ?(max_error = 0.0)
    ?options
    ?src_wkt ?dst_wkt
    ?(create_options = [])
    src ~filename driver alg =
  let options =
    match options with
    | None -> from_voidp Options.Raw.t null
    | Some { Options.o; _ } -> o
  in
  let create_options = Lib.convert_creation_options create_options in
  create_and_reproject_image src src_wkt filename dst_wkt driver
    (Lib.creation_options_to_ptr create_options)
    (int_of_resample alg) memory_limit max_error null null options

let auto_create_warped_vrt =
  Lib.c "GDALAutoCreateWarpedVRT"
    (Data_set.t @-> string_opt @-> string_opt @-> int @-> float @->
     ptr Options.Raw.t @-> returning Data_set.t_opt)

let auto_create_warped_vrt ?src_wkt ?dst_wkt ?(max_error = 0.0)
    ?options src alg =
  let options =
    match options with
    | None -> from_voidp Options.Raw.t null
    | Some { Options.o; _ } -> o
  in
  let result =
    auto_create_warped_vrt src src_wkt dst_wkt (int_of_resample alg) max_error
      options
  in
  match result with
  | Some ds -> ds
  | None -> raise Warp_error

type warp_output_t = {
  geo_transform : Geo_transform.t;
  dims : int * int;
}

let suggested_warp_output arg =
  Lib.c "GDALSuggestedWarpOutput"
    (Data_set.t @-> Foreign.funptr (Transform.transform_t arg) @-> arg @-> ptr double
     @-> ptr int @-> ptr int @-> returning err)

let suggested_warp_output ds transform =
  let transform_t = Transform.get_transform_t transform in
  let transform_f = Transform.get_transform_c transform in
  let geo_transform =
    Geo_transform.make
      ~origin:(0.0, 0.0) ~pixel_size:(0.0, 0.0) ~rotation:(0.0, 0.0)
  in
  let pixels = allocate_n int ~count:1 in
  let lines = allocate_n int ~count:1 in
  suggested_warp_output (ptr void) ds transform_f transform_t (
    let open Bigarray in
    bigarray_start array1
      (geo_transform :> (float, float64_elt, c_layout) Array1.t)
  ) pixels lines;
  { geo_transform; dims = !@pixels, !@lines }

module Operation = struct
  type raw_t = T.t
  let t = T.t

  type 'a t = {
    t : raw_t;
    options : 'a Options.t;
  }

  let create =
    Lib.c "GDALCreateWarpOperation"
      (ptr Options.Raw.t @-> returning t)

  let destroy =
    Lib.c "GDALDestroyWarpOperation"
      (t @-> returning void)

  let create options =
    let result = create Options.(options.o) in
    if result = null then
      raise Warp_error
    else (
      Gc.finalise destroy result;
      { t = result; options }
    )

  let chunk_and_warp_image =
    Lib.c "GDALChunkAndWarpImage"
      (t @-> int @-> int @-> int @-> int @-> returning err)

  let chunk_and_warp_image t ~offset ~size =
    chunk_and_warp_image t.t (fst offset) (snd offset) (fst size) (snd size)

  let chunk_and_warp_multi =
    Lib.c "GDALChunkAndWarpMulti"
      (t @-> int @-> int @-> int @-> int @-> returning err)

  let chunk_and_warp_multi t ~offset ~size =
    chunk_and_warp_multi t.t (fst offset) (snd offset) (fst size) (snd size)

  let warp_region =
    Lib.c "GDALWarpRegion"
      (t @-> int @-> int @-> int @-> int @-> int @-> int @-> int @-> int @->
       returning err)

  let warp_region t ~dst_offset ~dst_size ~src_offset ~src_size =
    let dox, doy = dst_offset in
    let dsx, dsy = dst_size in
    let sox, soy = src_offset in
    let ssx, ssy = src_size in
    warp_region t.t dox doy dsx dsy sox soy ssx ssy

  let warp_region_to_buffer =
    Lib.c "GDALWarpRegionToBuffer"
      (t @-> int @-> int @-> int @-> int @->
       ptr void @-> int @->
       int @-> int @-> int @-> int @->
       returning err)

  let warp_region_to_buffer
      ?buffer t dt ~dst_offset ~dst_size ~src_offset ~src_size
    =
    let open Bigarray in
    let dox, doy = dst_offset in
    let dsx, dsy = dst_size in
    let sox, soy = src_offset in
    let ssx, ssy = src_size in
    let buffer =
      match buffer with
      | Some b ->
        let rows = Array2.dim1 b in
        let cols = Array2.dim2 b in
        if cols < dox + dsx || rows < doy + dsy then
          invalid_arg "Buffer is too small"
        else
          b
      | None ->
        let kind = Band.Data.to_ba_kind dt in
        Array2.create kind c_layout (doy + dsy) (dox + dsx)
    in
    let buffer_ptr = bigarray_start array2 buffer in
    let dt_i = Band.Data.to_int dt in
    warp_region_to_buffer t.t dox doy dsx dsy (to_voidp buffer_ptr) dt_i
      sox soy ssx ssy;
    buffer

  let wrap ?offset ?size options f =
    let offset =
      match offset with
      | Some o -> o
      | None -> 0, 0
    in
    let size =
      match size with
      | Some s -> s
      | None ->
        let ds = getf !@Options.(options.o) Options.Raw.dst_ds in
        Data_set.get_x_size ds,
        Data_set.get_y_size ds
    in
    let operation = create options in
    f operation ~offset ~size

  let warp ?offset ?size options =
    wrap ?offset ?size options chunk_and_warp_image

  let warp_multi ?offset ?size options =
    wrap ?offset ?size options chunk_and_warp_multi
end
