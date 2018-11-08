(** [Jerboa.Request_handler] contains only the type definnition of a request handler.*)

(** [Request_handler.t] is the type definition of the handler, which will handle the request and produce a response.*)
type t = Request.t -> Response.t