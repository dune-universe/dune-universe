type ns = N | S
type ew = E | W
type lat = float * ns
type lng = float * ew

type t = lat * lng
(** latlng type *)

val lat : t -> lat
(** [lat ll] returns longitude *)

val lng : t -> lng
(** [lng ll] returns longitude *)

val to_string : t -> string
(** [to_string ll] transforms latlong to readable string *)

val parse_lat : float -> ns -> lat
(** [parse_lat d ew] parses nmea latitude *)

val parse_lng : float -> ew -> lng
(** [parse_lng d ew] parses nmea longitude *)