open! Core

type t =
  | Normal_closure
  (**
     1000 indicates a normal closure, meaning that the purpose for
     which the connection was established has been fulfilled.
  *)
  | Endpoint_going_away
  (**
     1001 indicates that an endpoint is "going away", such as a server
     going down or a browser having navigated away from a page.
  *)
  | Protocol_error
  (**
     1002 indicates that an endpoint is terminating the connection due
     to a protocol error.
  *)
  | Cannot_accept_data
  (**
     1003 indicates that an endpoint is terminating the connection
     because it has received a type of data it cannot accept (e.g., an
     endpoint that understands only text data MAY send this if it
     receives a binary message).
  *)
  | Reserved_0
  (**
     Reserved.  The specific meaning might be defined in the future.
  *)
  | No_status_code
  (**
     1005 is a reserved value and MUST NOT be set as a status code in a
     Close control frame by an endpoint.  It is designated for use in
     applications expecting a status code to indicate that no status
     code was actually present.
  *)
  | Closed_abnormally
  (**
     1006 is a reserved value and MUST NOT be set as a status code in a
     Close control frame by an endpoint.  It is designated for use in
     applications expecting a status code to indicate that the
     connection was closed abnormally, e.g., without sending or
     receiving a Close control frame.
  *)
  | Invalid_message_sent
  (**
     1007 indicates that an endpoint is terminating the connection
     because it has received data within a message that was not
     consistent with the type of the message (e.g., non-UTF-8 [RFC3629]
     data within a text message).
  *)
  | Policy_violation
  (**
     1008 indicates that an endpoint is terminating the connection
     because it has received a message that violates its policy.  This
     is a generic status code that can be returned when there is no
     other more suitable status code (e.g., 1003 or 1009) or if there
     is a need to hide specific details about the policy.
  *)
  | Message_too_large
  (**
     1009 indicates that an endpoint is terminating the connection
     because it has received a message that is too big for it to
     process.
  *)
  | Invalid_handshake
  (**
     1010 indicates that an endpoint (client) is terminating the
     connection because it has expected the server to negotiate one or
     more extension, but the server didn't return them in the response
     message of the WebSocket handshake.  The list of extensions that
     are needed SHOULD appear in the /reason/ part of the Close frame.
     Note that this status code is not used by the server, because it
     can fail the WebSocket handshake instead.
  *)
  | Unexpected_condition
  (**
     1011 indicates that a server is terminating the connection because
     it encountered an unexpected condition that prevented it from
     fulfilling the request.
  *)
  | Tls_handshake_failure
  (**
     1015 is a reserved value and MUST NOT be set as a status code in a
     Close control frame by an endpoint.  It is designated for use in
     applications expecting a status code to indicate that the
     connection was closed due to a failure to perform a TLS handshake
     (e.g., the server certificate can't be verified).
  *)
[@@deriving sexp_of]

val to_int : t -> int
val of_int : int -> t Or_error.t
