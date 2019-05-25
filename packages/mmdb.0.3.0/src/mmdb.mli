(** Binding to the maxminddb library which parses the MMDB format commonly known as GeoIP2 *)

(** Reference to an MMBD file *)
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

module Fetch_ip_data_error : sig
  type t =
    [ `Invalid_address_info
    | `Ipv6_lookup_in_ipv4_database of string
    | Common_error.t ]
  [@@deriving show]
end

module Fetch_value_error : sig
  type t =
    [ `Invalid_lookup_path of string
    | `Invalid_node_number of string
    | `Unsupported_data_type of string
    | `Unexpected_data_type of string
    | Common_error.t ]
  [@@deriving show]
end

module Fetch_error : sig
  type t =
    [ Fetch_ip_data_error.t
    | Fetch_value_error.t ]
  [@@deriving show]
end

(** The version string of the underlying C library *)
val library_version : string

module Path = Types.Path

(** Open an MMDB file and return a reference to it *)
val open_file : Path.t -> (t, Open_file_error.t) result

module Version_number = Types.Version_number

(** The version number of the binary format in underlying MMDB file *)
val binary_format_version : t -> Version_number.t

module Language = Types.Language

(** Retrieves a list of languages supported by the database *)
val languages : t -> Language.t list

(** Retrieves a specific language by its language code *)
val language_by_code : t -> string -> Language.t option

module Ip = Types.Ip

(** An opaque reference to the data structure associated with an IP address *)
type ip_data

(** Retrieves the data associated with the supplied IP address *)
val fetch_ip_data : t -> Ip.t -> (ip_data, Fetch_ip_data_error.t) result

(** Signatures for handling queries that yield a particular type of answer *)
module type VALUE_TYPE = sig
  module Query : sig
    (** Represents a query for this value type *)
    type t

    (** Construct a query from a path within *)
    val of_string_list : string list -> t

    (** Return the path for  *)
    val to_string_list : t -> string list
  end

  (** The type of answer returned by a query *)
  type answer

  (** Fetches a value directly from the database *)
  val from_db : t -> Ip.t -> Query.t -> (answer option, Fetch_error.t) result

  (** Fetches a value from an {!ip_data} reference *)
  val from_ip_data : ip_data -> Query.t -> (answer option, Fetch_value_error.t) result
end

(** The supported atomic value types that can be retrieved from a database *)
type any_value =
  | String of string
  | Float of float
  | Int of int
  | Bool of bool

(** Interface to query for values of type {!any_value}. This may come useful
    in case the database contains different value types at the same query path.
    Specialized modules for retrieving strings, floats, integers and booleans
    are available below. *)
module Any : VALUE_TYPE with type answer = any_value

(** Interface for retrieving string values from the database *)
module String : sig
  include VALUE_TYPE with type answer = string

  (** Query that determines the code of the country where the IP is located *)
  val country_code : Query.t

  (** Query that determines the code of the region where the IP is located *)
  val region_code : Query.t

  (** Creates a query that retrieves the localized name of the city where the IP is located *)
  val city_name : Language.t -> Query.t

  (** Creates a query that retrieves the localized name of the country where the IP is located *)
  val country_name : Language.t -> Query.t

  (** Creates a query that retrieves the localized name of the continent where the IP is located *)
  val continent_name : Language.t -> Query.t
end

(** Interface for retrieving float values from the database *)
module Float : VALUE_TYPE with type answer = float

(** Interface for retrieving integer values from the database *)
module Int : VALUE_TYPE with type answer = int

(** Interface for retrieving boolean values from the database *)
module Bool : VALUE_TYPE with type answer = bool

(** Interface for retrieving coordinate values from the database *)
module Coordinates : sig
  include module type of Coordinates

  include VALUE_TYPE with type answer = Coordinates.t

  (** Query that determines the geographical location of an IP *)
  val location : Query.t
end
