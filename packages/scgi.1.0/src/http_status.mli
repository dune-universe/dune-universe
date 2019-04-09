type t =
  [ `Accepted
  | `Bad_gateway
  | `Bad_request
  | `Conflict
  | `Created
  | `Custom_code of int * string
  | `Expectation_failed
  | `Forbidden
  | `Found
  | `Gateway_timeout
  | `Gone
  | `Http_version_not_supported
  | `Internal_server_error
  | `Length_required
  | `Method_not_allowed
  | `Moved_permanently
  | `Multiple_choices
  | `No_content
  | `Non_authoritative_information
  | `Not_acceptable
  | `Not_found
  | `Not_implemented
  | `Not_modified
  | `Ok
  | `Partial_content
  | `Payment_required
  | `Precondition_failed
  | `Proxy_authentication_required
  | `Request_entity_too_large
  | `Request_timeout
  | `Request_uri_too_long
  | `Requested_range_not_satisfiable
  | `Reset_content
  | `See_other
  | `Service_unavailable
  | `Temporary_redirect
  | `Unauthorized
  | `Unsupported_media_type ]

val of_pair : int * string -> t

val to_pair : t -> int * string

val to_int : t -> int

val to_string : t -> string

(**/**)

val values : t -> int * string

(* deprecated; same as to_pair *)
