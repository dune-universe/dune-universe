module type Sender =
  sig
    type t
    val close : t -> unit
    val write : t -> bytes -> int -> int -> int option
  end

module Make :
  functor (S: Sender) ->
    sig
      type t
      val default_bufsize : int
      val default_conn_timeout : int
      val of_string : string -> [> `FixRaw of char list ]
      val uint8_of_int : 'a -> [> `Uint8 of 'a ]
      val uint16_of_int : 'a -> [> `Uint16 of 'a ]
      val uint32_of_int : int -> [> `Uint32 of int64 ]
      val uint64_of_int : int -> [> `Uint64 of int64 ]
      val int8_of_int : 'a -> [> `Int8 of 'a ]
      val int16_of_int : 'a -> [> `Int16 of 'a ]
      val int32_of_int : int -> [> `Int32 of int64 ]
      val int64_of_int : int -> [> `Int64 of int64 ]
      val of_float : 'a -> [> `Float of 'a ]
      val of_double : 'a -> [> `Double of 'a ]
      val post_with_time : t -> string -> Msgpack.Serialize.t -> int64 -> bool
      val post : t -> string -> Msgpack.Serialize.t -> bool
      val release : t -> unit
      val create_with_sender : ?bufsize:int -> S.t -> t
      val init : S.t -> int -> t
    end

module Default :
  sig
    type t
    val default_bufsize : int
    val default_conn_timeout : int
    val of_string : string -> [> `FixRaw of char list ]
    val uint8_of_int : 'a -> [> `Uint8 of 'a ]
    val uint16_of_int : 'a -> [> `Uint16 of 'a ]
    val uint32_of_int : int -> [> `Uint32 of int64 ]
    val uint64_of_int : int -> [> `Uint64 of int64 ]
    val int8_of_int : 'a -> [> `Int8 of 'a ]
    val int16_of_int : 'a -> [> `Int16 of 'a ]
    val int32_of_int : int -> [> `Int32 of int64 ]
    val int64_of_int : int -> [> `Int64 of int64 ]
    val of_float : 'a -> [> `Float of 'a ]
    val of_double : 'a -> [> `Double of 'a ]
    val post_with_time : t -> string -> Msgpack.Serialize.t -> int64 -> bool
    val post : t -> string -> Msgpack.Serialize.t -> bool
    val release : t -> unit
    val create_with_sender : ?bufsize:int -> Stream_sender.t -> t
  end

include module type of Default
val create_for_inet : ?bufsize:int -> ?conn_timeout:int -> ?host:string -> ?port:int -> unit -> t
val create_for_unix : ?bufsize:int -> ?conn_timeout:int -> string -> t
val create : ?bufsize:int -> ?conn_timeout:int -> ?host:string -> ?port:int -> unit -> t

