(** Functions for dealing with responses from requests. *)

(** The response from a request. The [request] field contains details about the actual request sent (for instance, it includes headers added by Quests, and the URL will have a query string with any supplied parameters. *)
type t = {
  content: string;
  status_code: int;
  headers: Cohttp.Header.t;
  request: Request.t;
}

val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

val show : t -> string

val content : t -> string

val status_code : t -> int

val headers : t -> Cohttp.Header.t

val ok : t -> bool
(** Returns whether a request has been successful. *)

val json : t -> Yojson.Safe.t
(** Parses the body of a response into JSON. *)

val result_for_status : t -> (t, t) result
(** Creates a result from the response depending on whether it was successful. *)
