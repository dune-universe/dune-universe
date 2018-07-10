open Ctypes

exception Invalid_transform

type data_t = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
type result_t =
  (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t option

type 'a transform_t =
  'a -> int -> int -> float ptr -> float ptr -> float ptr -> int ptr -> int

let ( !! ) (gt : Geo_transform.t) = bigarray_start array1 (gt :> data_t)

let transform_t t =
  t @->
  int @-> int @->
  ptr double @-> ptr double @-> ptr double @->
  ptr int @->
  returning int

let transform_c name t =
  Lib.c name (transform_t t)

let transform_ml c t forward (xs : data_t) (ys : data_t) (zs : data_t) =
  let n =
    let open Bigarray in
    let nx = Array1.dim xs in
    let ny = Array1.dim ys in
    let nz = Array1.dim zs in
    if nx = ny && nx = nz then
      nx
    else
      invalid_arg "Coordinate mismatch"
  in
  let success =
    let open Bigarray in
    let a = Array1.create int c_layout n in
    Array1.fill a 1;
    a
  in
  let result =
    c t (if forward then 1 else 0) n
      (bigarray_start array1 xs)
      (bigarray_start array1 ys)
      (bigarray_start array1 zs)
      (bigarray_start array1 success)
  in
  if result = 0 then
    None
  else
    Some success

module Gen_img = struct
  type t = T.t
  let t = T.t
  let t_opt = T.t_opt

  let create =
    Lib.c "GDALCreateGenImgProjTransformer"
      (Data_set.t_opt @-> string_opt @-> Data_set.t_opt @-> string_opt @->
       int @-> double @-> int @->
       returning t_opt)

  let create3 =
    Lib.c "GDALCreateGenImgProjTransformer3"
      (string @-> ptr double @-> string @-> ptr double @-> returning t_opt)

  let set_dst_geo_transform =
    Lib.c "GDALSetGenImgProjTransformerDstGeoTransform"
      (t @-> ptr double @-> returning void)

  let c =
    transform_c "GDALGenImgProjTransform" t

  let ml t =
    transform_ml c t

  let create_data_set src dst gcp_ok gcp_order =
    create (Some src) None (Some dst) None gcp_ok 0.0 gcp_order

  let create_data_set_wkt src dst_wkt gcp_ok gcp_order =
    create (Some src) None None (Some dst_wkt) gcp_ok 0.0 gcp_order

  let create_wkt_data_set src dst gcp_ok gcp_order =
    create None (Some src) (Some dst) None gcp_ok 0.0 gcp_order

  let create_wkt src_wkt src_geo_transform dst_wkt dst_geo_transform =
    create3 src_wkt !!src_geo_transform dst_wkt !!dst_geo_transform

  let create ?gcp kind =
    let gcp_ok, gcp_order =
      match gcp with
      | None -> true, 0
      | Some g -> g
    in
    let gcp_ok = if gcp_ok then 1 else 0 in
    let t =
      match kind with
      | `data_set (src, dst) -> create_data_set src dst gcp_ok gcp_order
      | `wkt ((src_wkt, src_gt), (dst_wkt, dst_gt)) ->
        create_wkt src_wkt src_gt dst_wkt dst_gt
      | `data_set_wkt (src, dst_wkt) ->
        create_data_set_wkt src dst_wkt gcp_ok gcp_order
      | `wkt_data_set (src, dst) ->
        create_wkt_data_set src dst gcp_ok gcp_order
    in
    match t with
    | None -> raise Invalid_transform
    | Some t -> t
end

module Reprojection = struct
  type t = T.t
  let t = T.t

  let create =
    Lib.c "GDALCreateReprojectionTransformer"
      (string @-> string @-> returning t)

  let destroy =
    Lib.c "GDALDestroyReprojectionTransformer"
      (t @-> returning void)

  let c =
    transform_c "GDALReprojectionTransform" t

  let ml t =
    transform_ml c t

  let create ~src ~dst =
    let t = create src dst in
    if t = null then raise Invalid_transform;
    Gc.finalise destroy t;
    t
end

type image
type reprojection

type _ t =
  | Gen_img : Gen_img.t -> image t
  | Repojection : Reprojection.t -> reprojection t

let make_gen_img ?gcp kind =
  let t = Gen_img.create ?gcp kind in
  Gen_img t

let set_dst_geo_transform (Gen_img t) (gt : Geo_transform.t) =
  Gen_img.set_dst_geo_transform t !!gt

let make_reprojection ~src ~dst =
  let t = Reprojection.create ~src ~dst in
  Repojection t

let transform (type s) (t : s t) =
  match t with
  | Gen_img g -> Gen_img.ml g
  | Repojection r -> Reprojection.ml r

let get_transform_t (type s) (t : s t) =
  match t with
  | Gen_img g -> g
  | Repojection r -> r

let get_transform_c (type s) (t : s t) =
  match t with
  | Gen_img _ -> Gen_img.c
  | Repojection _ -> Reprojection.c
