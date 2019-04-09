(** SCGI client *)

val request_inet :
  server_name:string -> port:int -> Request.t -> Response.t Lwt.t

val request_sock : socket_filename:string -> Request.t -> Response.t Lwt.t
