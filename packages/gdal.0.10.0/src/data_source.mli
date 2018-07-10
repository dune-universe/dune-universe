(** {1 OGR Data Sources} *)

(** These are bindings and wrappers around the [OGR_DS_*] API. *)

type t
(** Data source *)

exception Data_source_error

type driver_t
(** OGR driver *)

val get_driver_by_name : string -> driver_t option
val get_driver_by_name_exn : string -> driver_t
(** [get_driver_by_name name] returns the driver associated with [name]. *)

val create : ?options:string list -> driver_t -> string -> t option
val create_exn : ?options:string list -> driver_t -> string -> t
(** [create ?options driver name] *)

val copy : ?options:string list -> driver_t -> t -> string -> t option
val copy_exn : ?options:string list -> driver_t -> t -> string -> t
(** [copy ?options driver src name] creates a copy of [src] using [driver]. *)

val of_source : ?write:bool -> string -> [ `Invalid_source | `Ok of t ]
(** [of_source ?write name] opens the source [name] for access.

    @param write defaults to false (read-only)
    @param name is source-type specific.  See the upstream OGR documentation
           for more information.

    @return `Invalid_source if [name] does not represent a valid data source. *)

val of_source_exn : ?write:bool -> string -> t
(** Like {!of_source} but raises {!Invalid_source} if there is an error with
    the data source. *)

val destroy : t -> unit
(** [destroy t] releases the resources associated with [t]. *)

val with_source :
  ?write:bool ->
  string ->
  (t -> 'a) ->
  [ `Invalid_source | `Ok of 'a ]
(** [with_source ?write name f] opens [name] and calls [f src] if [name] is a
    valid data source.  The data source passed to [f] will be closed if [f]
    returns normally or raises an exception.

    This is a wrapper around {!of_source}.  See its documentation for a
    description of the expected arguments. *)

val with_source_exn : ?write:bool -> string -> (t -> 'a) -> 'a
(** Like {!with_source} but raises {!Invalid_source} if there is an error with
    the data source. *)

val get_layer_by_name : t -> string -> Layer.t
val get_layer : t -> int -> Layer.t
(** [get_layer* src id] returns a {!Layer.t} extracted from [src]. *)

val get_layer_count : t -> int
(** [get_layer_count src] returns the number of layers in [src]. *)

val create_layer :
  ?spatial_reference:Spatial_reference.t ->
  ?geometry_type:Geometry.wkb_t ->
  ?options:string list ->
  t -> string -> Layer.t option
(** [create_layer ?spatial_reference ?geometry_type ?options ds name] creates a
    layer named [name] in [ds].

    @param spatial_reference defaults to no spatial reference.
    @param geometry_type defaults to {!Geometry.Unknown}
    @param options defaults to an empty list.

    @return [Some layer] on success or [None] if the layer could not be
    created. *)

val create_layer_exn :
  ?spatial_reference:Spatial_reference.t ->
  ?geometry_type:Geometry.wkb_t ->
  ?options:string list ->
  t -> string -> Layer.t
(** [create_layer_exn ?spatial_reference ?geometry_type ?options ds name] is
    like {!create_layer} except that it raises {!Data_source_error} if the
    layer can not be created. *)

val copy_layer :
  ?options:string list -> t -> Layer.t -> string -> Layer.t option
(** [copy_layer ?options ds src name] copies [src] to a layer named [name] in
    [ds].

    @param options defaults to an empty list.

    @return [Some layer] on success or [None] if the layer could not be
    copied/created. *)

val copy_layer_exn : ?options:string list -> t -> Layer.t -> string -> Layer.t
(** [copy_layer_exn ?options ds src name] is like {!copy_layer} except that it
    raises {!Data_source_error} if the layer can not be copied. *)

(**/**)

val t : t Ctypes.typ

(** {2 Low level wrappers} *)

(** These should not be necessary under normal circumstances. *)

val release : t -> unit
(** @raise Data_source_error *)
