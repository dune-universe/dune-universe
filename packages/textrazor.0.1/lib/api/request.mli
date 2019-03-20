(** Sends a [GET] request to the given URI and API key. *)
val get : Uri.t -> string -> (Yojson.Safe.json, string) result

(** Sends a [POST] request to the given URI and API key.

    The request has a [Content-type: x-www-form-urlencoded] header and its body
    is constructed from [params].
*)
val post_form : Uri.t -> ?params:(string * string list) list -> string
  -> (Yojson.Safe.json, string) result