open Async
open Async_ssl.Std

type t = {
  s : ([ `Active ], Socket.Address.Inet.t) Socket.t;
  ssl : Ssl.Connection.t option;
  r : Reader.t;
  w : Writer.t;
}

val is_tls_url : Uri.t -> bool

val connect :
  ?version:Async_ssl.Version.t ->
  ?options:Async_ssl.Opt.t list ->
  ?socket:([ `Unconnected ], Socket.Address.Inet.t) Socket.t ->
  (Uri.t -> t Deferred.t) Tcp.with_connect_options

val with_connection :
  ?version:Async_ssl.Version.t ->
  ?options:Async_ssl.Opt.t list ->
  (url:Uri.t -> f:(t -> 'a Deferred.t) -> unit -> 'a Deferred.t)
  Tcp.with_connect_options

module Persistent :
  Persistent_connection_kernel.S with type conn := t and type address = Uri.t

val listen_ssl :
  ?version:Async_ssl.Version.t ->
  ?options:Async_ssl.Opt.t list ->
  ?name:string ->
  ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ] ->
  ?ca_file:string ->
  ?ca_path:string ->
  ?verify_modes:Async_ssl.Verify_mode.t list ->
  crt_file:string ->
  key_file:string ->
  ?buffer_age_limit:Writer.buffer_age_limit ->
  ?max_connections:int ->
  ?max_accepts_per_batch:int ->
  ?backlog:int ->
  ?socket:([ `Unconnected ], [< Socket.Address.t ] as 'a) Socket.t ->
  on_handler_error:[ `Call of 'a -> exn -> unit | `Ignore | `Raise ] ->
  ('a, 'b) Tcp.Where_to_listen.t ->
  ('a -> Ssl.Connection.t -> Reader.t -> Writer.t -> unit Deferred.t) ->
  ('a, 'b) Tcp.Server.t Deferred.t
