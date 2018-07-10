(** {1 GDAL Data Sets} *)

type t
val t : t Ctypes.typ
val t_opt : t option Ctypes.typ
(** Data set *)

exception Invalid_source
exception Invalid_projection
exception Band_error
exception Copy_error
exception Overview_error
exception Wrong_data_type

val of_source :
  ?write:bool ->
  string ->
  [ `Error of [> `Invalid_source ] | `Ok of t ]
(** [of_source ?write name] opens the source [name] for access.

    @param write defaults to false (read-only)
    @param name is source-type specific.  See the upstream GDAL documentation
           for more information.

    @return `Invalid_source if [name] does not represent a valid data source. *)

val of_source_exn : ?write:bool -> string -> t
(** Like {!of_source} except that this function raises {!Invalid_source} if
    there is an error. *)

val close : t -> unit
(** [close t] closes the data set [t]. *)

val with_source :
  ?write:bool ->
  string ->
  (t -> ([> `Error of [> `Invalid_source ] ] as 'a)) -> 'a
(** [with_source ?write name f] opens [name] and calls [f src] if [name] is a
    valid data source.  The data source passed to [f] will be closed if [f]
    returns normally or raises an exception.

    This is a wrapper around {!of_source}.  See its documentation for a
    description of the expected arguments. *)

val with_source_exn : ?write:bool -> string -> (t -> 'a) -> 'a
(** Like {!with_source} except that this function raises {!Invalid_source}
    if there is an error when trying to open the data source. *)

val get_driver : t -> Driver.t
(** [get_driver t] returns the driver associated with [t]. *)

val get_projection : t -> string
(** [get_projection t] returns a string representing the projection applied to
    [t]. *)

val get_x_size : t -> int
val get_y_size : t -> int
(** [get_x/y_size t] returns the [x] or [y] dimension of [t]'s rasters. *)

val get_count : t -> int
(** [get_count t] returns number of raster bands in [t]. *)

val get_band : t -> int -> ('v, 'e) Band.Data.t -> ('v, 'e) Band.t
(** [get_band t i kind] returns the [i]th raster band from [t].

    @param i is 1-based, not 0-based
    @param kind is the native data type of the band
    @raise Wrong_data_type if [kind] is not correct *)

val get_band_data_type : t -> int -> [
    `byte
  | `uint16
  | `int16
  | `uint32
  | `int32
  | `float32
  | `float64
  | `unknown
  | `unhandled
  ]
(** [get_band_data_type t i] returns the native data type of the [i]th band in
    [t]. *)

val add_band : ?options:string list -> t -> ('v, 'e) Band.Data.t -> unit
(** [add_band ?options t kind] adds a band of type [kind] to [t]. *)

val create_copy :
  ?strict:bool ->
  ?options:string list ->
  t -> Driver.t -> string ->
  [ `Error of [ `Invalid_source ] | `Ok of t ]
(** [create_copy ?strict ?options t driver name] creates a copy of [t].

    @param driver specifies the driver to use for the copy. *)

val create_copy_exn :
  ?strict:bool -> ?options:string list -> t -> Driver.t -> string -> t
(** Like {!create_copy} except that the function raises {!Invalid_source} if
    there is an error. *)

val create :
  ?options:string list ->
  ?bands:int * (_, _) Band.Data.t ->
  Driver.t -> string -> int * int ->
  [ `Error of [ `Invalid_source ] | `Ok of t ]
(** [create ?options ?bands driver name size] creates a new {!t} with the
    given specifications.

    @param size specifies the [(x, y)] dimensions of bands in pixels
    @param bands specifies the number of bands to initialize in the data set
           and their data type *)

val create_exn :
  ?options:string list ->
  ?bands:int * (_, _) Band.Data.t ->
  Driver.t -> string -> int * int -> t
(** Like {!create} except that the function raises {!Invalid_source} if there
    is an error. *)

val copy : ?options:string list -> src:t -> dst:t -> unit
(** [copy ?options ~src ~dst] efficiently copies data from [src] to [dst].
    Number and dimensions of the included bands must match.  The data types of
    each band do not need to match. *)

val set_projection : t -> string -> unit
(** [set_project t wkt_projection] sets the projection for [t].  The projection
    string should be in WKT format. *)

val of_band : (_, _) Band.t -> t
(** [of_band band] returns the {!t} associated with [band]. *)

val build_overviews :
  ?bands:int list -> t -> int list -> string -> unit
(** [build_overviews ?bands t factors resampling] builds overviews for [t] on
    bands [bands] using decimation factors [factors] with resampling method
    [resampling]. *)
