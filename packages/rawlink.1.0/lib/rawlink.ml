(*
 * Copyright (c) 2015 Christiano F. Haesbaert <haesbaert@haesbaert.org>
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

[@@@warning "-32-37"]

[%%cstruct
type bpf_hdr = {
	bh_sec: uint32_t;
	bh_usec: uint32_t;
	bh_caplen: uint32_t;
	bh_datalen: uint32_t;
	bh_hdrlen: uint16_t;
} [@@little_endian]]

type t = {
  fd : Unix.file_descr;
  packets : Cstruct.t list ref;
  buffer : Cstruct.t;
}

type driver =
  | AF_PACKET
  | BPF

external opensock: ?filter:string -> string -> Unix.file_descr = "caml_rawlink_open"
external dhcp_server_filter: unit -> string = "caml_dhcp_server_filter"
external dhcp_client_filter: unit -> string = "caml_dhcp_client_filter"
external driver: unit -> driver = "caml_driver"
external unix_bytes_read: Unix.file_descr -> Cstruct.buffer -> int -> int -> int =
  "lwt_unix_bytes_read"
external bpf_align: int -> int -> int = "caml_bpf_align"

let open_link ?filter ifname =
  { fd = opensock ?filter:filter ifname;
    packets = ref [];
    buffer = Cstruct.create 65536 }

let close_link t = Unix.close t.fd

let send_packet t buf =
  let len = Cstruct.len buf in
  let n = Unix.write t.fd (Cstruct.to_bytes buf) 0 len in
  if n = 0 then
    raise (Unix.Unix_error(Unix.EPIPE, "send_packet: socket closed", ""))
  else if n <> len then
    raise (Unix.Unix_error(Unix.ENOBUFS, "send_packet: short write", ""))

let bpf_split_buffer buffer len =
  let rec loop buffer n packets =
    if n <= 0 then
      List.rev packets
    else
      let bh_caplen = Int32.to_int (get_bpf_hdr_bh_caplen buffer) in
      let bh_datalen = Int32.to_int (get_bpf_hdr_bh_datalen buffer) in
      let bh_hdrlen = get_bpf_hdr_bh_hdrlen buffer in
      let nextoff = bpf_align bh_hdrlen bh_caplen in
      if bh_caplen <> bh_datalen then
        loop (Cstruct.shift buffer nextoff) (n - nextoff) packets
      else
        let pkt = Cstruct.create bh_datalen in
        Cstruct.blit buffer bh_hdrlen pkt 0 bh_datalen;
        loop (Cstruct.shift buffer nextoff) (n - nextoff) (pkt :: packets)
  in
  loop buffer len []

let rec read_packet t =
  match !(t.packets) with
  | hd :: tl -> t.packets := tl; hd
  | [] -> match driver () with
    | BPF ->
      let n = unix_bytes_read t.fd t.buffer.Cstruct.buffer 0 t.buffer.Cstruct.len in
      if n = 0 then
        failwith "Link socket closed";
      t.packets := bpf_split_buffer t.buffer n;
      read_packet t
    | AF_PACKET ->
      let n = unix_bytes_read t.fd t.buffer.Cstruct.buffer 0 t.buffer.Cstruct.len in
      if n = 0 then
        failwith "Link socket closed";
      let buf = Cstruct.create n in
      Cstruct.blit t.buffer 0 buf 0 n;
      buf
