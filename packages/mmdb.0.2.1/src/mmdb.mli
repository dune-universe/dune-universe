(** Binding to the maxminddb library which parses the MMDB format commonly known as GeoIP2 *)

(** Reference to a MMBD file *)
type t

(** Thrown when an error is detected that is an internal error of the library, not a
    usage error. It is recommended not to handle this, instead report a bug report in
    the library *)
exception Binding_integrity_error of string

module Common_error : sig
  type t =
    [ `Corrupt_search_tree of string
    | `Io_error of string
    | `Out_of_memory of string
    | `Invalid_data of string ]
  [@@deriving show]
end

module Open_file_error : sig
  type t =
    [ `File_open_error of string
    | `Invalid_metadata of string
    | `Unknown_database_format of string
    | Common_error.t ]
  [@@deriving show]
end

module Lookup_ip_error : sig
  type t =
    [ `Invalid_address_info
    | `Ipv6_lookup_in_ipv4_database of string
    | Common_error.t ]
  [@@deriving show]
end

module Lookup_error : sig
  type t = [`Unsupported_data_type of string | Lookup_ip_error.t]
  [@@deriving show]
end

module Lookup_result : sig
  type 'a t = ('a option, Lookup_error.t) result
end

module Path = Types.Path
module Ip = Types.Ip
module Coordinates = Coordinates

(** Open an MMDB file and return a reference to it *)
val open_file : Path.t -> (t, Open_file_error.t) result

(** Determine the coordinates of an IP *)
val coordinates : t -> Ip.t -> Coordinates.t Lookup_result.t

(** Determine the country code of an IP *)
val country_code : t -> Ip.t -> string Lookup_result.t

(** Determine the region an IP is in *)
val region_code : t -> Ip.t -> string Lookup_result.t
