open Async
open Async_ssl.Std

val is_tls_url : Uri.t -> bool

val connect :
  ?version:Async_ssl.Version.t ->
  ?options:Async_ssl.Opt.t list ->
  ?socket:([ `Unconnected ], Socket.Address.Inet.t) Socket.t ->
  (Uri.t ->
   (([ `Active ], Socket.Address.Inet.t) Socket.t *
    Ssl.Connection.t option * Reader.t * Writer.t) Deferred.t)
    Tcp.with_connect_options

val with_connection :
  ?version:Async_ssl.Version.t ->
  ?options:Async_ssl.Opt.t list ->
  (Uri.t ->
   ((([ `Active ], Socket.Address.Inet.t) Socket.t ->
     Ssl.Connection.t option ->
     Reader.t -> Writer.t -> 'a Deferred.t) ->
    'a Deferred.t)) Tcp.with_connect_options

val listen_ssl :
  ?buffer_age_limit:Writer.buffer_age_limit ->
  ?max_connections:int ->
  ?max_accepts_per_batch:int ->
  ?backlog:int ->
  ?socket:([ `Unconnected ], [< Socket.Address.t ] as 'a) Socket.t ->
  ?version:Async_ssl.Version.t ->
  ?options:Async_ssl.Opt.t list ->
  ?name:string ->
  ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ] ->
  ?ca_file:string ->
  ?ca_path:string ->
  ?verify_modes:Async_ssl.Verify_mode.t list ->
  crt_file:string ->
  key_file:string ->
  on_handler_error:[ `Call of 'a -> exn -> unit | `Ignore | `Raise ] ->
  ('a, 'b) Tcp.Where_to_listen.t ->
  ('a ->
   Ssl.Connection.t -> Reader.t -> Writer.t -> unit Deferred.t) ->
  ('a, 'b) Tcp.Server.t Deferred.t
