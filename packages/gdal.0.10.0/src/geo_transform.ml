open Ctypes

exception Geo_transform_error

let err = T.err Geo_transform_error

type t = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

let allocate_gt () =
  Bigarray.(Array1.create float64 c_layout 6)

let to_ptr gt = bigarray_start array1 gt

let get =
  Lib.c "GDALGetGeoTransform"
    (Data_set.t @-> ptr double @-> returning err)

let get t =
  let ba = allocate_gt () in
  get t (bigarray_start array1 ba);
  ba

let get_origin gt =
  gt.{0}, gt.{3}

let get_pixel_size gt =
  gt.{1}, gt.{5}

let get_rotation gt =
  gt.{2}, gt.{4}

let set =
  Lib.c "GDALSetGeoTransform"
    (Data_set.t @-> ptr double @-> returning err)

let make ~origin ~pixel_size ~rotation =
  Bigarray.Array1.of_array Bigarray.float64 Bigarray.c_layout [|
    fst origin;
    fst pixel_size;
    fst rotation;
    snd origin;
    snd rotation;
    snd pixel_size;
  |]

let set t gt =
  set t (to_ptr gt)

let apply =
  Lib.c "GDALApplyGeoTransform"
    (ptr double @->
     double @-> double @->
     ptr double @-> ptr double @->
     returning void)

let apply gt ~x ~y =
  let cx = allocate double 0.0 in
  let cy = allocate double 0.0 in
  apply (to_ptr gt) x y cx cy;
  !@cx, !@cy

let invert =
  Lib.c "GDALInvGeoTransform"
    (ptr double @-> ptr double @-> returning int)

let invert gt =
  let gt_inv = allocate_gt () in
  let result = invert (to_ptr gt) (to_ptr gt_inv) in
  if result = 0 then
    raise Geo_transform_error
  else
    gt_inv
