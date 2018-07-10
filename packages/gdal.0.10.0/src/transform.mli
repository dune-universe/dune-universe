exception Invalid_transform

type data_t =
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Input coordinates are provided as bigarrays. *)

type result_t =
  (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t option
(** Success/failure of coordinate transformations. *)

type image
type reprojection

type 'a t
(** Transformation *)

val make_gen_img :
  ?gcp:bool * int ->
  [
    `data_set of Data_set.t * Data_set.t |
    `wkt of (string * Geo_transform.t) * (string * Geo_transform.t) |
    `data_set_wkt of Data_set.t * string |
    `wkt_data_set of string * Data_set.t
  ] ->
  image t
(** [make_gen_img ?gcp kind] creates a transformation defined by [kind].

    Options for [kind]:
    - [`data_set (src, dst)]
    - [`wkt ((src_wkt, src_geo_transform), (dst_wkt, dst_geo_transform))]
      {i uses [GDALCreateGenImgTransformer3] internally, [gcp] is ignored}
    - [`data_set_wkt (src, (dst_wkt, dst_geo_transform))]
    - [`wkt_data_set (src_wkt, dst)]

    @param gcp defaults to [(true, 0)].  See GDAL's
    [GDALCreateGenImgProjTransformer] documentation for an explanation of how
    GCPs may be used. *)

val set_dst_geo_transform : image t -> Geo_transform.t -> unit
(** [set_dst_geo_transform t gt] sets the destination geo transform matrix in
    [t]. *)

val make_reprojection : src:string -> dst:string -> reprojection t
(** [make_reprojection ~src ~dst] creates a transformation definition between
    the WKT definition in [src] and the WKT definition in [dst]. *)

val transform : _ t -> bool -> data_t -> data_t -> data_t -> result_t
(** [transform t invert xs ys zs] converts the coordinates [xs], [ys], [zs]
    according to the transformation defined in [t].

    @param invert reverses the direction of the transformation [t].
    @param xs is modified in place with the transformation result.
    @param ys is modified in place with the transformation result.
    @param zs is modified in place with the transformation result.

    @return [Some success] where [success] is an array of values indicating
    whether an individual point's transformation is successful or not.  [None]
    is returned if the overall transformation fails. *)

(**/**)

(** Ctype support functions for use from other modules *)

open Ctypes

type 'a transform_t =
  'a -> int -> int -> float ptr -> float ptr -> float ptr -> int ptr -> int

val transform_t : 'a typ -> 'a transform_t fn
val get_transform_t : _ t -> unit ptr
val get_transform_c : _ t -> unit ptr transform_t
