(** Data structure for client-level configuration. *)
type t = {
  secure: bool; (** Whether to use SSL when querying TextRazor. *)
  use_eu_endpoint: bool; (** Whether to use the European endpoint only, for GPDR
    purposes. *)
}

(** Creates a new configuration.

    Default values are [{secure = true; use_eu_endpoint = false}].
*)
val create : ?secure:bool -> ?use_eu_endpoint:bool -> unit -> t

(** Builds a TextRazor URL according to the given configuration. *)
val url: t -> ?path:string -> ?query:(string * string list) list -> unit
  -> Uri.t