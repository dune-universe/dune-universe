(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)


open Nbd

type error = int32

type handle = int64

type size = int64

let rec really_read fd string off n =
	if n=0 then () else
	let m = Unix.read fd string off n in
	if m = 0 then raise End_of_file;
	really_read fd string (off+m) (n-m)

let really_read_string fd length =
	let buf = String.make length '\000' in
	really_read fd buf 0 length;
	buf

let really_write fd string off n =
	let written = ref 0 in
	while !written < n
	do
		let wr = Unix.write fd string (off + !written) (n - !written) in
		written := wr + !written
	done

(* Ideally, really_write would be implemented with optional arguments ?(off=0) ?(len=String.length string) *)
let really_write_string fd string =
	really_write fd string 0 (String.length string)

let negotiate sock = 
	if really_read_string sock 8 <> init_passwd then failwith "Bad magic in negotiate/1";
	let bs = get_int64 (Bitstring.bitstring_of_file_descr_max sock 8) in
	if bs=opts_magic then
		failwith "Unhandled opts_magic"
	else if bs<>cliserv_magic then
		failwith "Bad magic";
	let sz = get_int64 (Bitstring.bitstring_of_file_descr_max sock 8) in
	let flags =  get_int32 (Bitstring.bitstring_of_file_descr_max sock 4) in
	let _ = Bitstring.bitstring_of_file_descr_max sock 124 in
	(sz,flags_of_flags (Int32.to_int flags))

let read sock from len =
	let request = {
		Request.ty = NBD_cmd_read;
		handle = 0L;
		from=from;
		len=len } in
	let msg = construct_request request in
	really_write_string sock msg;
	let reply = Bitstring.bitstring_of_file_descr_max sock 16 in
	let parsed = parse_reply reply in
	if parsed.Reply.error=0l then 
		Some (really_read_string sock (Int32.to_int len))
	else
		None

let disconnect_async sock rid =
	let request = {
		Request.ty=NBD_cmd_disc;
		handle=rid;
		from=0L;
		len=0l
	} in
	let msg = construct_request request in
	really_write_string sock msg

let write_async sock dest_ofs str ofs len rid =
	let request = {
		Request.ty = NBD_cmd_write;
		handle=rid;
		from=dest_ofs;
		len=Int32.of_int len;
	} in
	let msg = construct_request request in
	really_write_string sock msg;
	really_write sock str ofs len

let write_wait sock =
	let reply = Bitstring.bitstring_of_file_descr_max sock 16 in
	let parsed = parse_reply reply in
	if parsed.Reply.error=0l then 
		parsed.Reply.handle, None
	else
		parsed.Reply.handle, Some parsed.Reply.error

let write sock dest_ofs str ofs len =
	write_async sock dest_ofs str ofs len 0L;
	snd (write_wait sock)

let connect hostname port =
	let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	let host_info = Unix.gethostbyname hostname in
	let server_address = host_info.Unix.h_addr_list.(0) in
	let _ = Unix.connect socket (Unix.ADDR_INET (server_address, port)) in
	let (sz,flags) = negotiate socket in
	(socket, sz, flags)
