(** Non-comprehensive list of HTTP response status codes and strings from
    http://en.wikipedia.org/wiki/List_of_HTTP_status_codes *)
type t =
  [ `Ok
  | `Created
  | `Accepted
  | `Non_authoritative_information
  | `No_content
  | `Reset_content
  | `Partial_content
  | `Multiple_choices
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Temporary_redirect
  | `Bad_request
  | `Unauthorized
  | `Payment_required
  | `Forbidden
  | `Not_found
  | `Method_not_allowed
  | `Not_acceptable
  | `Proxy_authentication_required
  | `Request_timeout
  | `Conflict
  | `Gone
  | `Length_required
  | `Precondition_failed
  | `Request_entity_too_large
  | `Request_uri_too_long
  | `Unsupported_media_type
  | `Requested_range_not_satisfiable
  | `Expectation_failed
  | `Internal_server_error
  | `Not_implemented
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout
  | `Http_version_not_supported
  | `Custom_code of int * string ]

let to_pair = function
  | `Ok ->
      (200, "OK")
  | `Created ->
      (201, "Created")
  | `Accepted ->
      (202, "Accepted")
  | `Non_authoritative_information ->
      (203, "Non-Authoritative Information")
  | `No_content ->
      (204, "No Content")
  | `Reset_content ->
      (205, "Reset Content")
  | `Partial_content ->
      (206, "Partial Content")
  | `Multiple_choices ->
      (300, "Multiple Choices")
  | `Moved_permanently ->
      (301, "Moved Permanently")
  | `Found ->
      (302, "Found")
  | `See_other ->
      (303, "See Other")
  | `Not_modified ->
      (304, "Not Modified")
  | `Temporary_redirect ->
      (307, "Temporary Redirect")
  | `Bad_request ->
      (400, "Bad Request")
  | `Unauthorized ->
      (401, "Unauthorized")
  | `Payment_required ->
      (402, "Payment Required")
  | `Forbidden ->
      (403, "Forbidden")
  | `Not_found ->
      (404, "Not Found")
  | `Method_not_allowed ->
      (405, "Method Not Allowed")
  | `Not_acceptable ->
      (406, "Not Acceptable")
  | `Proxy_authentication_required ->
      (407, "Proxy Authentication Required")
  | `Request_timeout ->
      (408, "Request Timeout")
  | `Conflict ->
      (409, "Conflict")
  | `Gone ->
      (410, "Gone")
  | `Length_required ->
      (411, "Length Required")
  | `Precondition_failed ->
      (412, "Precondition Failed")
  | `Request_entity_too_large ->
      (413, "Request Entity Too Large")
  | `Request_uri_too_long ->
      (414, "Request URI Too Long")
  | `Unsupported_media_type ->
      (415, "Unsupported Media Type")
  | `Requested_range_not_satisfiable ->
      (416, "Request Range Not Satisfiable")
  | `Expectation_failed ->
      (417, "Expectation Failed")
  | `Internal_server_error ->
      (500, "Internal Server Error")
  | `Not_implemented ->
      (501, "Not Implemented")
  | `Bad_gateway ->
      (502, "Bad Gateway")
  | `Service_unavailable ->
      (503, "Service Unavailable")
  | `Gateway_timeout ->
      (504, "Gateway Timeout")
  | `Http_version_not_supported ->
      (505, "HTTP Version Not Supported")
  | `Custom_code (code, name) ->
      (code, name)

let values = to_pair

let to_int v = fst (to_pair v)

let to_string v = snd (to_pair v)

let of_pair (code, reason) =
  match code with
  | 200 ->
      `Ok
  | 201 ->
      `Created
  | 202 ->
      `Accepted
  | 203 ->
      `Non_authoritative_information
  | 204 ->
      `No_content
  | 205 ->
      `Reset_content
  | 206 ->
      `Partial_content
  | 300 ->
      `Multiple_choices
  | 301 ->
      `Moved_permanently
  | 302 ->
      `Found
  | 303 ->
      `See_other
  | 304 ->
      `Not_modified
  | 307 ->
      `Temporary_redirect
  | 400 ->
      `Bad_request
  | 401 ->
      `Unauthorized
  | 402 ->
      `Payment_required
  | 403 ->
      `Forbidden
  | 404 ->
      `Not_found
  | 405 ->
      `Method_not_allowed
  | 406 ->
      `Not_acceptable
  | 407 ->
      `Proxy_authentication_required
  | 408 ->
      `Request_timeout
  | 409 ->
      `Conflict
  | 410 ->
      `Gone
  | 411 ->
      `Length_required
  | 412 ->
      `Precondition_failed
  | 413 ->
      `Request_entity_too_large
  | 414 ->
      `Request_uri_too_long
  | 415 ->
      `Unsupported_media_type
  | 416 ->
      `Requested_range_not_satisfiable
  | 417 ->
      `Expectation_failed
  | 500 ->
      `Internal_server_error
  | 501 ->
      `Not_implemented
  | 502 ->
      `Bad_gateway
  | 503 ->
      `Service_unavailable
  | 504 ->
      `Gateway_timeout
  | 505 ->
      `Http_version_not_supported
  | code ->
      `Custom_code (code, reason)
