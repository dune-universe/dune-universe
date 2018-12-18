(*
  Copyright (C) iNuron - info@openvstorage.com
  This file is part of Open vStorage. For license information, see <LICENSE.txt>
 *)

open Kinetic_tag
open Kinetic_util

type off = int
type len = int
type 'a slice = 'a * off * len

type key = bytes
type version = bytes option
type timeout_ms = int64

module type INTEGRATION = sig
  type value
  type socket
  val create : int -> value
  val show : value -> string
  val show_socket : socket -> string
  val read  : socket -> value -> off -> len -> int Lwt.t
  val write : socket -> value -> off -> len -> int Lwt.t

  val read_bytes  : socket -> Bytes.t -> off -> len -> int Lwt.t
  val write_bytes : socket -> Bytes.t -> off -> len -> int Lwt.t

  val make_sha1 : value -> off -> len -> Tag.t
  val make_crc32: value -> off -> len -> Tag.t
end

module BytesIntegration = struct
  type value = bytes
  type socket = Lwt_ssl.socket

  let show_socket socket =
    let fd = Lwt_ssl.get_unix_fd socket in
    let (fdi:int) = Obj.magic fd in
    string_of_int fdi

  let create = Bytes.create
  let show = trimmed

  let read socket  = Lwt_ssl.read  socket
  let write socket = Lwt_ssl.write socket

  let read_bytes = read
  let write_bytes = write

  let make_sha1 v_buff v_off v_len  =
    let h = Cryptokit.Hash.sha1() in
    let () = h # add_substring v_buff v_off v_len in
    let hrs = h # result in
    let hr = Bytes.of_string hrs in
    Tag.Sha1 hr

  let make_crc32 _ _ _ = failwith "todo: BytesValue.make_crc32"
end
