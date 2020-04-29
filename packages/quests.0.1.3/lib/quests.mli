(** Quests provides functions {!get}, {!post}, {!put} and so on for making requests. You can also call {!request} with a specified method.

The same API is used in {!Session} to make requests with persistent connections. *)

val request :
  Cohttp.Code.meth ->
  ?params:(string * string) list ->
  ?data:Request.payload ->
  ?headers:(string * string) list ->
  ?auth:Request.authentication ->
  ?follow_redirects:bool ->
  string ->
  Response.t Lwt.t
(** [request meth ?params ?data ?headers ?auth ~follow_redirects url] makes a request to [url] with method [meth], a query string specified by [params], a body specified by [data], additional headers specified by [headers], authentication specified by [auth], and whether redirects should be followed automatically by [follow_redirects] (which defaults to [true]). *)

val get :
  ?params:(string * string) list ->
  ?data:Request.payload ->
  ?headers:(string * string) list ->
  ?auth:Request.authentication ->
  ?follow_redirects:bool ->
  string ->
  Response.t Lwt.t

val post :
  ?params:(string * string) list ->
  ?data:Request.payload ->
  ?headers:(string * string) list ->
  ?auth:Request.authentication ->
  ?follow_redirects:bool ->
  string ->
  Response.t Lwt.t

val put :
  ?params:(string * string) list ->
  ?data:Request.payload ->
  ?headers:(string * string) list ->
  ?auth:Request.authentication ->
  ?follow_redirects:bool ->
  string ->
  Response.t Lwt.t

val delete :
  ?params:(string * string) list ->
  ?data:Request.payload ->
  ?headers:(string * string) list ->
  ?auth:Request.authentication ->
  ?follow_redirects:bool ->
  string ->
  Response.t Lwt.t

module Request = Request
module Response = Response
module Session = Session
