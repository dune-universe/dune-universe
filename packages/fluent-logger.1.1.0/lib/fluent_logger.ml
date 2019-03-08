module type Sender =
  sig
    type t
    val close: t -> unit
    val write: t -> bytes -> int -> int -> int option
  end;;

module Make(S : Sender) :
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
=
  struct
    type t = {
      sender:S.t;
      buf:Buffer.t;
      mutable buf_pos:int;
    }

    let default_bufsize = 8 * 1024 * 1024

    let default_conn_timeout = 3

    let close logger = S.close logger.sender

    let flush_buf logger =
      let buflen = Buffer.length logger.buf in
      let rec _write () =
        if logger.buf_pos >= buflen then (
          logger.buf_pos <- 0;
          Buffer.clear logger.buf;
          true
        )
        else (
          let result = S.write logger.sender
                        (Buffer.to_bytes logger.buf)
                        logger.buf_pos
                        (buflen - logger.buf_pos) in
          match result with
          | Some len -> (
            logger.buf_pos <- logger.buf_pos + len;
            _write ()
          )
          | None -> (
            prerr_endline "flush_buf: write error";
            S.close logger.sender;
            false
          )
        )
      in
      _write ()

    let of_string s = `FixRaw (ExtString.String.explode s)

    let uint8_of_int i = `Uint8 i

    let uint16_of_int i = `Uint16 i

    let uint32_of_int i = `Uint32 (Int64.of_int i)

    let uint64_of_int i = `Uint64 (Int64.of_int i)

    let int8_of_int i = `Int8 i

    let int16_of_int i = `Int16 i

    let int32_of_int i = `Int32 (Int64.of_int i)

    let int64_of_int i = `Int64 (Int64.of_int i)

    let of_float f = `Float f

    let of_double d = `Double d

    let post_with_time logger tag record time =
      let packed = 
        let open Msgpack in
        let tag = of_string tag in
        Serialize.serialize_string
          (`FixArray [tag; `Uint32 time; record]) in
      Buffer.add_string logger.buf packed;
      flush_buf logger

    let post logger tag record =
      post_with_time logger tag record (Int64.of_float (Unix.time ()))

    let release logger =
      if not (flush_buf logger) then
        prerr_endline "release post failed";
      close logger

    let init sender bufsize =
      {
        sender=sender;
        buf=Buffer.create bufsize;
        buf_pos=0;
      }

    let create_with_sender ?(bufsize = default_bufsize) sender =
      init sender bufsize

  end;;

module Default = Make(Stream_sender)

include Default

let default_host = "localhost"

let default_port = 24224

let create_for_inet ?(bufsize = default_bufsize)
  ?(conn_timeout = default_conn_timeout)
  ?(host = default_host) ?(port = default_port) () =
  let sender =
    Stream_sender.create (Stream_sender.INET(host, port)) conn_timeout in
  init sender bufsize

let create_for_unix ?(bufsize = default_bufsize)
  ?(conn_timeout = default_conn_timeout) path =
  let sender =
    Stream_sender.create (Stream_sender.UNIX(path)) conn_timeout in
  init sender bufsize

let create = create_for_inet
