val big_one : unit -> unit
(** add [@@deriving conv{..}]. No inliners. *)

val splitted : string -> unit
(** add [@@deriving s], [@@deriving s_of], [@@deriving of_s]. Inliners are supported. *)
