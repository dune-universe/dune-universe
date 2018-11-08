(** Precision represents the different precisions supported for timestamps *)
module Precision : sig
  type t =
    | Nanosecond
    | Microsecond
    | Millisecond
    | Second
    | Minute 
    | Hour

  (** Returns the string of t using the InfluxDB line protocol *)
  val to_string : t -> string

  (** Creates t from a line protocol precision value. None if the string is invalid *)
  val of_string : string -> t option
end

(** TimestampNS represents a UNIX timestamp with up to nanosecond precision *)
module TimestampNS : sig
  type t = int64

  (** Creates a new t from a float value assumed to be in second precision *)
  val of_float_seconds : float -> t

  (** Returns a string value of t truncated to the provided precision *)
  val to_string_precision : Precision.t -> t -> string
end

(** Field represents a key value pair that can be attached to a Point *)
module Field : sig
  type field_key = string
  type field_value =
    | Float of float
    | Int of int
    | String of string
    | Bool of bool

  type t = (field_key * field_value)

  (** The field as a string in the InfluxDB line protocol *)
  val to_string : t -> string

  (** Field value to string *)
  val v_to_string : field_value -> string

  (** Create field with a float value *)
  val float : string -> float -> t

  (** Create field with a int value *)
  val int : string -> int -> t

  (** Create field with a string value *)
  val string : string -> string -> t

  (** Create field with a bool value *)
  val bool : string -> bool -> t

end

(** Point represents a single time series point *)
module Point : sig 
  type t = {
    name : string;
    (** The name of the series this point belongs to *)

    field : Field.t;
    (** At least one field is required, usually with a key of "value" *)
    tags : (string * string) list;
    (** Optional key / value pair tags *)
    extra_fields : Field.t list;
    (** Additional fields *)
    timestamp : TimestampNS.t option;
    (** If None, a timestamp will be assigned by InfluxDB  *)
  }

  (** Returns the the point in InfluxDB line protocol format  *)
  val to_line : ?precision:Precision.t -> t -> string

  (** Create a new Point *)
  val create : ?tags:(string * string) list -> ?extra_fields:Field.t list -> ?timestamp:TimestampNS.t -> field:Field.t -> string -> t
end

module Protocol : sig

  val header_build : string
  val header_version : string

  type ping_response = {
    build : string;
    version : string;
  }

end