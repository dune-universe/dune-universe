(** HTTP request method *)
type t = [ `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ]

val of_string : string -> t

val to_string : t -> string
