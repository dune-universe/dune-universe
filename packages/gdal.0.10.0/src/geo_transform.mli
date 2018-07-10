type t =
  private (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Pixel <-> coordinate transformation *)

val get : Data_set.t -> t
(** [get t] returns the geotransform array associated with [t]. *)

val get_origin : t -> float * float
(** [get_origin t] returns the [(x, y)] origin of rasters in [t]. *)

val get_pixel_size : t -> float * float
(** [get_pixel_size t] returns the [(x, y)] pixel size of the rasters in [t]. *)

val get_rotation : t -> float * float
(** [get_rotation t] returns the rotation of rasters in [t]. *)

val make :
  origin:float * float ->
  pixel_size:float * float ->
  rotation:float * float ->
  t
(** [make ~origin ~pixel_size ~rotation] creates a geotransform
    with the given specifications. *)

val set : Data_set.t -> t -> unit
(** [set t] sets the geotransform array for [t]. *)

val apply : t -> x:float -> y:float -> float * float
(** [apply t ~x ~y] returns [(x, y)] translated by [t], generally from native
    coordinates to pixel coordinates or from pixel coordinates to native
    coordinates.*)

val invert : t -> t
(** [invert t] inverts the geotransform [t]. *)
