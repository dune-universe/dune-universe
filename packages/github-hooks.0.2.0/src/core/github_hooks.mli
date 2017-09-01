(** Github Hooks *)

(** The type for TLS server configuration. Similar to
    [Conduit_lwt_unix.server_tls_config] *)
type tls_config =
  [ `Crt_file_path of string ] *
  [ `Key_file_path of string ] *
  [ `Password of bool -> string | `No_password ] *
  [ `Port of int ]

module Repo : sig
  type t = string * string
  module Set : Set.S with type elt = t
end

module type CONFIGURATION = sig
  module Log : Logs.LOG
  val secret_prefix : string
  val tls_config : (int -> tls_config) option
end

module type TIME = sig
  type t
  val min : t
  val now : unit -> t
end

module type SERVER = sig
  include Cohttp_lwt.S.Server
  type mode = private [> `TCP of [`Port of int] | `TLS of tls_config]
  val create: mode -> t -> unit Lwt.t
end

module type HOOKS = sig
  type t
  type token
  val create : token -> Uri.t -> t
  val run : t -> unit Lwt.t
  val repos : t -> Repo.Set.t
  val watch : t -> ?events:Github_t.event_type list -> Repo.t -> unit Lwt.t
  val events : t -> (Repo.t * Github_t.event_hook_constr) list
  val clear : t -> unit
  val wait : t -> unit Lwt.t
end

module Make
    (Time: TIME)
    (Github: Github_s.Github)
    (Server: SERVER)
    (Conf: CONFIGURATION):
  HOOKS with type token = Github.Token.t
