(** Various types to do with HTTP requests. *)

(** Represents data about a request. *)
type t = { url: string; headers: Cohttp.Header.t }

val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

val show : t -> string

(** Represents the kind of payloads that can be used in queries. *)
type payload =
  | Json of Yojson.t
  | Form of (string * string) list
  | Raw of string

(** Represents supported authentication methods. *)
type authentication = Basic of string * string | Bearer of string
