open Async
open Cohttp
open Cohttp_async

(** see [Azure Blob Storage API](https://docs.microsoft.com/en-us/rest/api/storageservices/blob-service-rest-api) *)

val sign_exn
  :  ?content_length:int64
  -> ?headers:Header.t
  -> meth:Code.meth
  -> path:string
  -> ?query:(string * string list) list
  -> Azblob.Conn.t
  -> Header.t * Uri.t

val body_length_exn : [> `Empty | `String of string | `Strings of string list ]
  -> int64

val get_blob
  :  ?interrupt:unit Deferred.t
  -> ?ssl_config:Conduit_async.V2.Ssl.Config.t
  -> ?headers:Header.t
  -> path:string
  -> Azblob.Conn.t
  -> (Response.t * Body.t) Deferred.t
(** [get_blob] downloads a blob. *)

val put_blob
  :  ?interrupt:unit Deferred.t
  -> ?ssl_config:Conduit_async.V2.Ssl.Config.t
  -> ?blob_type:[< `Append | `Block > `Block ]
  -> ?headers:Header.t
  -> path:string
  -> ?body:Body.t
  -> Azblob.Conn.t
  -> (Response.t * Body.t) Deferred.t
(** [put_blob] uploads a blob. Replace if already exists on same path. *)

val append_block
  :  ?interrupt:unit Deferred.t
  -> ?ssl_config:Conduit_async.V2.Ssl.Config.t
  -> ?headers:Header.t
  -> path:string
  -> ?body:Body.t
  -> Azblob.Conn.t
  -> (Response.t * Body.t) Deferred.t
(** [append_block] appends a blob.
    You should [put_blob] first with [blob_type] = [`Append]. *)

val delete_blob
  :  ?interrupt:unit Deferred.t
  -> ?ssl_config:Conduit_async.V2.Ssl.Config.t
  -> ?headers:Header.t
  -> path:string
  -> Azblob.Conn.t
  -> (Response.t * Body.t) Deferred.t
(** [delete_blob] deletes a blob. *)
