(** Sessions can be used to make multiple requests to a host that share the underlying TCP connection. Requests with sessions are made in the same way as ones without, with [Quests.get] replaced by [Session.get a_session].*)

type t

val create : ?max_pool_size:int -> unit -> t
(** Create a session with a maximum of [max_pool_size] connections per host. *)

val request :
  t ->
  Cohttp.Code.meth ->
  ?params:(string * string) list ->
  ?data:Request.payload ->
  ?headers:(string * string) list ->
  ?auth:Request.authentication ->
  ?follow_redirects:bool ->
  string ->
  Response.t Lwt.t

val get :
  t ->
  ?params:(string * string) list ->
  ?data:Request.payload ->
  ?headers:(string * string) list ->
  ?auth:Request.authentication ->
  ?follow_redirects:bool ->
  string ->
  Response.t Lwt.t

val post :
  t ->
  ?params:(string * string) list ->
  ?data:Request.payload ->
  ?headers:(string * string) list ->
  ?auth:Request.authentication ->
  ?follow_redirects:bool ->
  string ->
  Response.t Lwt.t

val put :
  t ->
  ?params:(string * string) list ->
  ?data:Request.payload ->
  ?headers:(string * string) list ->
  ?auth:Request.authentication ->
  ?follow_redirects:bool ->
  string ->
  Response.t Lwt.t

val delete :
  t ->
  ?params:(string * string) list ->
  ?data:Request.payload ->
  ?headers:(string * string) list ->
  ?auth:Request.authentication ->
  ?follow_redirects:bool ->
  string ->
  Response.t Lwt.t

val max_pool_size : t -> int

val reset : t -> unit Lwt.t
(** Close all connections in the session. *)

val close : t -> unit Lwt.t
(** A synonym for [reset]. *)
