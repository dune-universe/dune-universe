(** {1 Layers} *)

(** These functions come from OGR C API's [OGR_L_*] namespace. *)

type t

val t : t Ctypes.typ
val t_opt : t option Ctypes.typ

exception Layer_error

val get_name : t -> string
val get_geom_type : t -> Geometry.wkb_t
val get_spatial_filter : t -> Geometry.t
val set_spatial_filter : t -> Geometry.t -> unit
val set_spatial_filter_rect : t -> float -> float -> float -> float -> unit
val set_attribute_filter : t -> string -> unit
val reset_reading : t -> unit
val get_next_feature : t -> Feature.t option
val set_next_by_index : t -> int -> unit
val get_feature : t -> int -> Feature.t option
val set_feature : t -> Feature.t -> unit
val create_feature : t -> Feature.t -> unit
val delete_feature : t -> int -> unit
val get_layer_defn : t -> Feature.Defn.t
val get_spatial_ref : t -> Spatial_reference.t option
val get_feature_count : t -> int -> int
val get_extent : t -> int -> Geometry.envelope_t
val test_capability : t -> string -> bool
val create_field : t -> Field.Defn.t -> int -> unit
val delete_field : t -> int -> unit
val reorder_fields : t -> int list -> unit
val reorder_field : t -> int -> int -> unit
val alter_field_defn : t -> int -> Field.Defn.t -> int -> unit
val start_transaction : t -> unit
val commit_transaction : t -> unit
val rollback_transaction : t -> unit
val reference : t -> int
val dereference : t -> int
val get_ref_count : t -> int
val sync_to_disk : t -> unit
val get_fid_column : t -> string
val get_geometry_column : t -> string
val set_ignored_fields : t -> string option list -> unit

val map_features : t -> (Feature.t -> 'a) -> 'a list
val iter_features : t -> (Feature.t -> unit) -> unit
