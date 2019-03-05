type t

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
    | `Invalid_lookup_path of string
    | `Lookup_path_does_not_match_data of string
    | `Invalid_node_number of string
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

val open_file : Path.t -> (t, Open_file_error.t) result

val coordinates : t -> Ip.t -> Coordinates.t Lookup_result.t

val country_code : t -> Ip.t -> string Lookup_result.t

val region_code : t -> Ip.t -> string Lookup_result.t
