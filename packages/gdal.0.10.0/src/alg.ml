open Ctypes

exception Algorithm_error

let err = T.err Algorithm_error

let proximity =
  Lib.c "GDALComputeProximity"
    (Band.t @-> Band.t @-> ptr string_opt @-> ptr void @-> ptr void @-> returning err)

let proximity ?(options = []) ~src:(sc, _) ~test:(tc, _) =
  let options = Lib.convert_creation_options options in
  proximity sc tc (Lib.creation_options_to_ptr options) null null

let fill_nodata =
  Lib.c "GDALFillNoData" (
    Band.t @-> Band.t @->
    double @-> int @-> int @->
    ptr string_opt @->
    ptr void @-> ptr void @->
    returning err
  )

let fill_nodata ?(options = []) ~target:(tc, _) ~mask:(mc, _) search_distance smoothing_iterations =
  let options = Lib.convert_creation_options options in
  fill_nodata tc mc search_distance 0 smoothing_iterations
    (Lib.creation_options_to_ptr options) null null

let generate_contours =
  Lib.c "GDALContourGenerate" (
    Band.t @-> double @-> double @-> int @-> ptr double @->
    int @-> double @->
    Layer.t @-> int @-> int @-> ptr void @-> ptr void @-> returning err
  )

let generate_contours ?no_data ?id ?elevation band layer contours =
  let id = match id with Some x -> x | None -> ~-1 in
  let elevation = match elevation with Some x -> x | None -> ~-1 in
  let use_no_data, no_data =
    match no_data with
    | Some n -> true, n
    | None -> false, nan
  in
  let contour_base, contour_interval, levels =
    match contours with
    | `fixed l -> 0.0, 0.0, l
    | `interval (base, interval) -> base, interval, []
  in
  let levels = CArray.of_list double levels in
  let n_levels = CArray.length levels in
  let levels_ptr = CArray.start levels in
  let band = fst band in
  generate_contours band contour_interval contour_base n_levels
    levels_ptr (if use_no_data then 1 else 0) no_data
    layer id elevation null null

let split_list l =
  let rec loop l one two =
    match l with
    | [] -> List.rev one, List.rev two
    | (o, t) :: tl -> loop tl (o :: one) (t :: two)
  in
  loop l [] []

let rasterize_geometries =
  Lib.c "GDALRasterizeGeometries" (
    Data_set.t @-> int @-> ptr int @->
    int @-> ptr Geometry.t @->
    Foreign.funptr_opt (Transform.transform_t (ptr void)) @-> ptr void @->
    ptr double @->
    ptr string_opt @->
    ptr void @-> ptr void @-> returning err
  )

let rasterize_geometries ?transform ?(options = []) dataset bands geometries =
  let n_bands, bands =
    let a = CArray.of_list int bands in
    CArray.length a, a
  in
  let geometries, burn =
    let g, bs = split_list geometries in
    g, List.concat bs
  in
  let n_geoms, geoms =
    let a = CArray.of_list Geometry.t geometries in
    CArray.length a, a
  in
  let burn = CArray.of_list double burn in
  let transform_t, transform_c =
    match transform with
    | Some t -> Transform.get_transform_t t, Some (Transform.get_transform_c t)
    | None -> null, None
  in
  let options = Lib.convert_creation_options options in
  let bands_ptr = CArray.start bands in
  let geoms_ptr = CArray.start geoms in
  let burn_ptr = CArray.start burn in
  let options_ptr = Lib.creation_options_to_ptr options in
  rasterize_geometries
    dataset n_bands bands_ptr
    n_geoms geoms_ptr
    transform_c transform_t
    burn_ptr options_ptr
    null null

let rasterize_layers =
  Lib.c "GDALRasterizeLayers" (
    Data_set.t @-> int @-> ptr int @->
    int @-> ptr Layer.t @->
    Foreign.funptr_opt (Transform.transform_t (ptr void)) @-> ptr void @->
    ptr double @->
    ptr string_opt @->
    ptr void @-> ptr void @-> returning err
  )

let rasterize_layers ?transform ?(options = []) dataset bands layers =
  let n_bands, bands =
    let a = CArray.of_list int bands in
    CArray.length a, a
  in
  let layers, burn =
    let g, bs = split_list layers in
    g, List.concat bs
  in
  let n_lyrs, lyrs =
    let a = CArray.of_list Layer.t layers in
    CArray.length a, a
  in
  let burn = CArray.of_list double burn in
  let transform_t, transform_c =
    match transform with
    | Some t -> Transform.get_transform_t t, Some (Transform.get_transform_c t)
    | None -> null, None
  in
  let options = Lib.convert_creation_options options in
  let bands_ptr = CArray.start bands in
  let lyrs_ptr = CArray.start lyrs in
  let burn_ptr = CArray.start burn in
  let options_ptr = Lib.creation_options_to_ptr options in
  rasterize_layers
    dataset n_bands bands_ptr
    n_lyrs lyrs_ptr
    transform_c transform_t
    burn_ptr options_ptr
    null null

module Grid = struct
  type interpolate_t = {
    algorithm : int;
    options : (interpolate_t, [ `Struct ]) structured;
  }

  let inverse_distance_to_a_power
      ~power
      ~smoothing
      ~anisotropy_ratio
      ~anisotropy_angle
      ~radius
      ~angle
      ~points
      ~no_data_value
    =
    let options = structure "GDALGridInverseDistanceToAPowerOptions" in
    let add t l = field options l t in
    let dfPower = add double "dfPower" in
    let dfSmoothing = add double "dfSmoothing" in
    let dfAnisotropyRatio = add double "dfAnisotropyRatio" in
    let dfAnisotropyAngle = add double "dfAnisotropyAngle" in
    let dfRadius1 = add double "dfRadius1" in
    let dfRadius2 = add double "dfRadius2" in
    let dfAngle = add double "dfAngle" in
    let nMaxPoints = add uint32_t "nMaxPoints" in
    let nMinPoints = add uint32_t "nMinPoints" in
    let dfNoDataValue = add double "dfNoDataValue" in
    seal (options : interpolate_t structure typ);
    let o = make options in
    setf o dfPower power;
    setf o dfSmoothing smoothing;
    setf o dfAnisotropyRatio anisotropy_ratio;
    setf o dfAnisotropyAngle anisotropy_angle;
    setf o dfRadius1 (fst radius);
    setf o dfRadius2 (snd radius);
    setf o dfAngle angle;
    setf o nMaxPoints (Unsigned.UInt32.of_int (snd points));
    setf o nMinPoints (Unsigned.UInt32.of_int (fst points));
    setf o dfNoDataValue no_data_value;
    { algorithm = 1; options = o }

  let moving_average
      ~radius
      ~angle
      ~min_points
      ~no_data_value
    =
    let options = structure "GDALGridMovingAverageOptions" in
    let add t l = field options l t in
    let dfRadius1 = add double "dfRadius1" in
    let dfRadius2 = add double "dfRadius2" in
    let dfAngle = add double "dfAngle" in
    let nMinPoints = add uint32_t "nMinPoints" in
    let dfNoDataValue = add double "dfNoDataValue" in
    seal (options : interpolate_t structure typ);
    let o = make options in
    setf o dfRadius1 (fst radius);
    setf o dfRadius2 (snd radius);
    setf o dfAngle angle;
    setf o nMinPoints (Unsigned.UInt32.of_int min_points);
    setf o dfNoDataValue no_data_value;
    { algorithm = 2; options = o }

  let nearest_neighbor
      ~radius
      ~angle
      ~no_data_value
    =
    let options = structure "GDALGridNearestNeighborOptions" in
    let add t l = field options l t in
    let dfRadius1 = add double "dfRadius1" in
    let dfRadius2 = add double "dfRadius2" in
    let dfAngle = add double "dfAngle" in
    let dfNoDataValue = add double "dfNoDataValue" in
    seal (options : interpolate_t structure typ);
    let o = make options in
    setf o dfRadius1 (fst radius);
    setf o dfRadius2 (snd radius);
    setf o dfAngle angle;
    setf o dfNoDataValue no_data_value;
    { algorithm = 3; options = o }

  type metric_t =
    radius:float * float ->
    angle:float ->
    min_points:int ->
    no_data_value:float ->
    interpolate_t

  let metric algorithm
      ~radius
      ~angle
      ~min_points
      ~no_data_value
    =
    let options = structure "GDALGridDataMetricsOptions" in
    let add t l = field options l t in
    let dfRadius1 = add double "dfRadius1" in
    let dfRadius2 = add double "dfRadius2" in
    let dfAngle = add double "dfAngle" in
    let nMinPoints = add uint32_t "nMinPoints" in
    let dfNoDataValue = add double "dfNoDataValue" in
    seal (options : interpolate_t structure typ);
    let o = make options in
    setf o dfRadius1 (fst radius);
    setf o dfRadius2 (snd radius);
    setf o dfAngle angle;
    setf o nMinPoints (Unsigned.UInt32.of_int min_points);
    setf o dfNoDataValue no_data_value;
    { algorithm; options = o }

  let metric_minimum = metric 4
  let metric_maximum = metric 5
  let metric_range = metric 6
  let metric_count = metric 7
  let metric_average_distance = metric 8
  let metric_average_distance_points = metric 9

  let grid_create =
    Lib.c "GDALGridCreate" (
      int @-> ptr void @->
      uint32_t @-> ptr double @-> ptr double @-> ptr double @->
      double @-> double @-> double @-> double @->
      uint32_t @-> uint32_t @->
      int @->
      ptr void @->
      ptr void @-> ptr void @->
      returning err
    )

  let make
      interpolation points
      ~xrange:(nx, xmin, xmax)
      ~yrange:(ny, ymin, ymax) data_type =
    let xs, ys, zs, npts =
      List.fold_left (
        fun (xs, ys, zs, npts) (x, y, z) ->
          x :: xs, y :: ys, z :: zs, succ npts
      ) ([], [], [], 0) points
    in
    let ba =
      let open Bigarray in
      Array2.create (Band.Data.to_ba_kind data_type) c_layout nx ny
    in
    let xs = CArray.of_list double xs in
    let ys = CArray.of_list double ys in
    let zs = CArray.of_list double zs in
    let xs_ptr = CArray.start xs in
    let ys_ptr = CArray.start ys in
    let zs_ptr = CArray.start zs in
    let ba_ptr = bigarray_start array2 ba in
    let options_ptr = addr interpolation.options in
    grid_create
      interpolation.algorithm (to_voidp options_ptr)
      (Unsigned.UInt32.of_int npts) xs_ptr ys_ptr zs_ptr
      xmin xmax ymin ymax
      (Unsigned.UInt32.of_int nx) (Unsigned.UInt32.of_int ny)
      (Band.Data.to_int data_type)
      (to_voidp ba_ptr)
      null null;
    ba
end
