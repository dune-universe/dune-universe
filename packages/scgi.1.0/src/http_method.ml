(** HTTP request method *)
type t = [ `DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT ]

let of_string = function
  | "DELETE" ->
      `DELETE
  | "GET" ->
      `GET
  | "HEAD" ->
      `HEAD
  | "OPTIONS" ->
      `OPTIONS
  | "PATCH" ->
      `PATCH
  | "POST" ->
      `POST
  | "PUT" ->
      `PUT
  | s ->
      failwith ("Invalid request method: " ^ s)

let to_string = function
  | `DELETE ->
      "DELETE"
  | `GET ->
      "GET"
  | `HEAD ->
      "HEAD"
  | `OPTIONS ->
      "OPTIONS"
  | `PATCH ->
      "PATCH"
  | `POST ->
      "POST"
  | `PUT ->
      "PUT"
