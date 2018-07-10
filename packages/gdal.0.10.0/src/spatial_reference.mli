(** {1 Spatial References} *)

(** These functions come from OGR C API's [OSR*] namespace. *)

type t
val t : t Ctypes.typ
val t_opt : t option Ctypes.typ

exception Spatial_reference_error

val make : [ `proj4 | `wkt | `name ] -> string -> t
(** [make kind def] will create a new {!t} from [def].

    @param kind Specifies what kind of definition is in [def]. [`proj4] for a
    PROJ.4 defintion, [`wkt] for a Well Known Text definition or [`name] for a
    well known name definition (ex. ["EPSG:4326"] or ["WGS84"]).
    @raise Spatial_reference_error if [def] is not a valid definition of
           type [kind]. *)

val to_proj4 : t -> string
(** [to_proj4 t] returns the PROJ.4 string definition matching [t]. *)

val to_wkt : ?pretty:bool -> ?simplify:bool -> t -> string
(** [to_wkt ?pretty ?simplify t] returns the WKT string definition matching
    [t]. *)
