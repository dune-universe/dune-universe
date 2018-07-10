(** {1 GDAL Algorithms} *)

exception Algorithm_error

val proximity : ?options:string list -> src:(_, _) Band.t -> test:(_, _) Band.t -> unit
(** [proximity ?options ~src ~test] computes the pixel-by-pixel proximity
    between [src] and [test]. *)

val fill_nodata :
  ?options:string list ->
  target:(_, _) Band.t -> mask:(_, _) Band.t -> float -> int -> unit
(** [fill_nodata ?options ~target ~mask distance iterations] will fill in
    [target]'s missing data pixels.  See GDAL's [GDALFillNoData] documentation
    for an explanation of the function parameters. *)

val generate_contours :
  ?no_data:float ->
  ?id:int ->
  ?elevation:int ->
  ('v, 'e) Band.t ->
  Layer.t ->
  [ `fixed of float list | `interval of float * float ] ->
  unit
(** [generate_contours ?no_data ?id ?elevation band layer contours] will
    create contours from [band], adding them to [layer].

    @param no_data will be used as the "no data" value if specified.
    @param id specifies a field where an identifier for each contour written
    to [layer].
    @param elevation specifies a field where the elevation value for each
    contour is written.
    @param band is the field to generate contours from.
    @param layer is the layer where contour geometries will be written to.
    @param contours specifies the contours to create.  [`fixed l] will create
    contours for each value in [l].  [`interval (start, step)] will create
    intervals starting with [start] and [step] intervals past that. *)

val rasterize_geometries :
  ?transform:'a Transform.t ->
  ?options:string list ->
  Data_set.t -> int list ->
  (Geometry.t * float list) list ->
  unit
(** [rasterize_geometries ?transform ?options ds bands geometries burn] will
    will rasterize [geometries] onto [ds].

    @param ds is the data set where output is written.
    @param bands specifies the list of bands to update.
    @param geometries is the list of geometries to burn in and the values to
    burn, one per entry in [bands].
    @param transform specifies the transformation to pixel/line coordinates.
    Not required if [ds] and [geometries] use the same coordinates.
    @param options specifies rasterization options.  See the documentation
    for [GDALRasterizeGeometries] for available options.  Defaults to [[]].

    @raise Invalid_argument if the length of the burn value list(s) do not
    match the length of the band list. *)

val rasterize_layers :
  ?transform:'a Transform.t ->
  ?options:string list ->
  Data_set.t -> int list ->
  (Layer.t * float list) list ->
  unit
(** [rasterize_layers ?transform ?options ds bands layers burn] will
    will rasterize [layers] onto [ds].

    @param ds is the data set where output is written.
    @param bands specifies the list of bands to update.
    @param layers is the list of layers to burn in and the values to
    burn, one per entry in [bands].
    @param transform specifies the transformation to pixel/line coordinates.
    Not required if [ds] and [layers] use the same coordinates.
    @param options specifies rasterization options.  See the documentation
    for [GDALRasterizeLayers] for available options.  Defaults to [[]].

    @raise Invalid_argument if the length of the burn value list(s) do not
    match the length of the band list. *)

module Grid : sig
  type interpolate_t

  val inverse_distance_to_a_power :
    power:float ->
    smoothing:float ->
    anisotropy_ratio:float ->
    anisotropy_angle:float ->
    radius:float * float ->
    angle:float ->
    points:int * int ->
    no_data_value:float ->
    interpolate_t

  val moving_average :
    radius:float * float ->
    angle:float ->
    min_points:int ->
    no_data_value:float ->
    interpolate_t

  val nearest_neighbor :
    radius:float * float ->
    angle:float ->
    no_data_value:float ->
    interpolate_t

  type metric_t =
    radius:float * float ->
    angle:float ->
    min_points:int ->
    no_data_value:float ->
    interpolate_t

  val metric_minimum : metric_t
  val metric_maximum : metric_t
  val metric_range : metric_t
  val metric_count : metric_t
  val metric_average_distance : metric_t
  val metric_average_distance_points : metric_t

  val make :
    interpolate_t ->
    (float * float * float) list ->
    xrange:int * float * float ->
    yrange:int * float * float -> ('v, 'e) Band.Data.t ->
    ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t
  (** [make interp points ~xrange ~yrange kind] interpolates the data in
      [points] onto a regular grid specified by [xrange] and [yrange].

      @param interp specifies the interpolation method to use.
      @param points is a list of [(x, y, z)] triples.
      @param xrange is a [(steps, min, max)] triple.
      @param yrange is a [(steps, min, max)] triple.
      @param kind specifies the data type to use for the resulting values. *)
end
