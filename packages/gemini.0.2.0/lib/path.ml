(** Path utilities for encoding them into uris or human readable formats. *)

(** The type of a uri path *)
type t = string list

(** Produces the string representation of a path
   suitable for a uri encoding. *)
let to_string (path:t) = sprintf "/%s"
    (String.concat ~sep:"/" path)

(** Summarize the path in a human readable format. *)
let to_summary ~has_subnames (path:t) =
  sprintf "Gemini %s Command%s"
    (String.concat ~sep:" " path)
    (match has_subnames with
     | true -> "s"
     | false -> ""
    )




