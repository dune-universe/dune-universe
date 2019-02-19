(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Sexplib.Conv

type interface_ref

module Raw = struct
  type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t = {
    iface: interface_ref;
    mac: string;
    mtu: int;
    max_packet_size: int;
  }

  external init : int -> t = "caml_init_vmnet"
  external set_event_handler : interface_ref -> unit = "caml_set_event_handler"
  external wait_for_event : interface_ref -> unit = "caml_wait_for_event"
  external caml_vmnet_read : interface_ref -> buf -> int -> int -> int = "caml_vmnet_read"
  external caml_vmnet_write : interface_ref -> buf -> int -> int -> int = "caml_vmnet_write"

  exception Return_code of int
  let _ = Callback.register_exception "vmnet_raw_return" (Return_code 0)
end

type error =
 | Failure
 | Mem_failure
 | Invalid_argument
 | Setup_incomplete
 | Invalid_access
 | Packet_too_big
 | Buffer_exhausted
 | Too_many_packets
 | Unknown of int [@@deriving sexp]

exception Error of error [@@deriving sexp]
exception Permission_denied
exception No_packets_waiting [@@deriving sexp]

let error_of_int =
  function
  | 1001 -> Failure
  | 1002 -> Mem_failure
  | 1003 -> Invalid_argument
  | 1004 -> Setup_incomplete
  | 1005 -> Invalid_access
  | 1006 -> Packet_too_big
  | 1007 -> Buffer_exhausted
  | 1008 -> Too_many_packets
  | err  -> Unknown err

type mode =
  | Host_mode
  | Shared_mode [@@deriving sexp]

type t = {
  iface: interface_ref sexp_opaque;
  name: string;
  mtu: int;
  mac: Macaddr_sexp.t;
  max_packet_size: int;
} [@@deriving sexp_of]

let mac {mac; _} = mac
let mtu {mtu; _} = mtu
let max_packet_size {max_packet_size; _} = max_packet_size

let iface_num = ref 0

let init ?(mode = Shared_mode) () =
  let mode =
    match mode with
    | Host_mode -> 1000
    | Shared_mode -> 1001
  in
  try
    let t = Raw.init mode in
    let name = Printf.sprintf "vmnet%d" !iface_num in
    incr iface_num;
    let mac = Macaddr.of_bytes_exn t.Raw.mac in
    let mtu = t.Raw.mtu in
    let max_packet_size = t.Raw.max_packet_size in
    { iface=t.Raw.iface; mac; mtu; max_packet_size; name }
  with Raw.Return_code r ->
    if r = 1001 && Unix.geteuid() <> 0
    then raise Permission_denied
    else raise (Error (error_of_int r))

let set_event_handler {iface; _} =
  Raw.set_event_handler iface

let wait_for_event {iface; _} =
  Raw.wait_for_event iface

let read {iface;_} c =
  let r = Raw.caml_vmnet_read iface c.Cstruct.buffer c.Cstruct.off c.Cstruct.len in
  match r with
  | 0 -> raise No_packets_waiting
  | len when len > 0 -> Cstruct.set_len c len
  | err -> raise (Error (error_of_int (err * (-1))))

let write {iface;_} c =
  Raw.caml_vmnet_write iface c.Cstruct.buffer c.Cstruct.off c.Cstruct.len
  |> function
  | len when len > 0 -> ()
  | err -> raise (Error (error_of_int (err * (-1))))
