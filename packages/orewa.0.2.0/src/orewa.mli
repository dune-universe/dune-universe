open Core
open Async

type t

type common_error =
  [ `Connection_closed
  | `Unexpected ]
[@@deriving show, eq]

val echo : t -> string -> (string, [> common_error]) Deferred.Result.t

type exist =
  | Always
  | Not_if_exists
  | Only_if_exists

val set
  :  t ->
  key:string ->
  ?expire:Time.Span.t ->
  ?exist:exist ->
  string ->
  (bool, [> common_error]) Deferred.Result.t

val get : t -> string -> (string option, [> common_error]) Deferred.Result.t

val getrange
  :  t ->
  start:int ->
  end':int ->
  string ->
  (string, [> common_error]) Deferred.Result.t

val getset
  :  t ->
  key:string ->
  string ->
  (string option, [> common_error]) Deferred.Result.t

val strlen : t -> string -> (int, [> common_error]) Deferred.Result.t

val mget : t -> string list -> (string option list, [> common_error]) Deferred.Result.t

val mset : t -> (string * string) list -> (unit, [> common_error]) Deferred.Result.t

val msetnx : t -> (string * string) list -> (bool, [> common_error]) Deferred.Result.t

val lpush : t -> key:string -> string -> (int, [> common_error]) Deferred.Result.t

val lrange
  :  t ->
  key:string ->
  start:int ->
  stop:int ->
  (string list, [> common_error]) Deferred.Result.t

val append : t -> key:string -> string -> (int, [> common_error]) Deferred.Result.t

val auth
  :  t ->
  string ->
  (unit, [> `Redis_error of string | common_error]) Deferred.Result.t

val bgrewriteaof : t -> (string, [> common_error]) Deferred.Result.t

val bgsave : t -> (string, [> common_error]) Deferred.Result.t

val bitcount
  :  t ->
  ?range:int * int ->
  string ->
  (int, [> common_error]) Deferred.Result.t

type overflow =
  | Wrap
  | Sat
  | Fail

type intsize =
  | Signed of int
  | Unsigned of int

type offset =
  | Absolute of int
  | Relative of int

type fieldop =
  | Get of intsize * offset
  | Set of intsize * offset * int
  | Incrby of intsize * offset * int

val bitfield
  :  t ->
  ?overflow:overflow ->
  string ->
  fieldop list ->
  (int option list, [> common_error]) Deferred.Result.t

type bitop =
  | AND
  | OR
  | XOR
  | NOT

val bitop
  :  t ->
  destkey:string ->
  ?keys:string list ->
  key:string ->
  bitop ->
  (int, [> common_error]) Deferred.Result.t

type bit =
  | Zero
  | One
[@@deriving show, eq]

val bitpos
  :  t ->
  ?start:int ->
  ?end':int ->
  string ->
  bit ->
  (int option, [> common_error]) Deferred.Result.t

val getbit : t -> string -> int -> (bit, [> common_error]) Deferred.Result.t

val setbit : t -> string -> int -> bit -> (bit, [> common_error]) Deferred.Result.t

val decr : t -> string -> (int, [> common_error]) Deferred.Result.t

val decrby : t -> string -> int -> (int, [> common_error]) Deferred.Result.t

val incr : t -> string -> (int, [> common_error]) Deferred.Result.t

val incrby : t -> string -> int -> (int, [> common_error]) Deferred.Result.t

val incrbyfloat : t -> string -> float -> (float, [> common_error]) Deferred.Result.t

val select : t -> int -> (unit, [> common_error]) Deferred.Result.t

val del : t -> ?keys:string list -> string -> (int, [> common_error]) Deferred.Result.t

val exists
  :  t ->
  ?keys:string list ->
  string ->
  (int, [> common_error]) Deferred.Result.t

val expire : t -> string -> Time.Span.t -> (int, [> common_error]) Deferred.Result.t

val expireat : t -> string -> Time.t -> (int, [> common_error]) Deferred.Result.t

val keys : t -> string -> (string list, [> common_error]) Deferred.Result.t

val scan : ?pattern:string -> ?count:int -> t -> string Pipe.Reader.t

val move : t -> string -> int -> (bool, [> common_error]) Deferred.Result.t

val persist : t -> string -> (bool, [> common_error]) Deferred.Result.t

val randomkey : t -> (string, [> common_error]) Deferred.Result.t

val rename : t -> string -> string -> (unit, [> common_error]) Deferred.Result.t

val renamenx : t -> key:string -> string -> (bool, [> common_error]) Deferred.Result.t

type order =
  | Asc
  | Desc

val sort
  :  t ->
  ?by:string ->
  ?limit:int * int ->
  ?get:string list ->
  ?order:order ->
  ?alpha:bool ->
  ?store:string ->
  string ->
  ([> `Count of int | `Sorted of string list], [> common_error]) Deferred.Result.t

val ttl
  :  t ->
  string ->
  ( Time.Span.t,
    [> `No_such_key of string | `Not_expiring of string | common_error] )
  Deferred.Result.t

val type' : t -> string -> (string option, [> common_error]) Deferred.Result.t

val dump : t -> string -> (string option, [> common_error]) Deferred.Result.t

val restore
  :  t ->
  key:string ->
  ?ttl:Time.Span.t ->
  ?replace:bool ->
  string ->
  (unit, [> common_error]) Deferred.Result.t

val connect : ?port:int -> host:string -> t Deferred.t

val close : t -> unit Deferred.t

val with_connection : ?port:int -> host:string -> (t -> 'a Deferred.t) -> 'a Deferred.t
